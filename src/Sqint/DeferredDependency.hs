{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Block a change that VALIDATEs a constraint or SETs NOT NULL on a table with
-- a registered deferred backfill without calling deferred.ensure_finished in the
-- same change — such a change passes on empty dev/CI tables and fails or drains
-- inline in production.
module Sqint.DeferredDependency (
    loadRegisteredBackfills,
    detectUnfinishedDependency,
    registrationsIn,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.String.Interpolate (iii)
import Data.Text qualified as T
import PgQuery qualified as Pg
import RIO
import Sqint.DeployTree (DeployFile (..))
import Sqint.Diagnostic (Finding (..), Severity (..), SourceRange (..))
import Sqint.Location (statementRange)
import Sqint.Rule (Rule (..))


-- | Backfills registered anywhere in the deploy tree, keyed by the
-- @schema.table@ they run against.
loadRegisteredBackfills :: [DeployFile] -> Map Text (Set Text)
loadRegisteredBackfills deployTree =
    Map.unionsWith Set.union (map (registrationsInLowered . (.lowered)) deployTree)


-- | The @(table, backfill name)@ pairs a deploy script registers, read textually
-- from each @insert into deferred.migration@. Values come from a lowercased copy:
-- fine for these two lowercase-identifier fields, not for the mixed-case
-- @enqueued_by@, which is deliberately not read here.
registrationsIn :: Text -> Map Text (Set Text)
registrationsIn = registrationsInLowered . T.toLower


-- | 'registrationsIn' for a caller that already holds a case-folded copy,
-- which is every file of a deploy tree read by "Sqint.DeployTree".
registrationsInLowered :: Text -> Map Text (Set Text)
registrationsInLowered src =
    Map.fromListWith
        Set.union
        [ (table, Set.singleton name)
        | stmt <- drop 1 (T.splitOn "insert into deferred.migration" src)
        , let body = T.takeWhile (/= ';') stmt
        , Just fields <- [insertedFields body]
        , Just name <- [Map.lookup "name" fields]
        , Just table <- [Map.lookup "source_table" fields]
        ]


-- | Pair a registration's column list with its @values@ row. An unreadable shape
-- yields nothing; the @prepare_migration@ trigger is the floor under this rule.
insertedFields :: Text -> Maybe (Map Text Text)
insertedFields body = do
    (columnList, afterColumns) <- parenGroup body
    (valueList, _) <- parenGroup (snd (T.breakOn "values" afterColumns))
    let columns = map T.strip (T.splitOn "," columnList)
        values = map (unquote . T.strip) (T.splitOn "," valueList)
    -- zip stops at the shorter, so a mismatched arity yields fewer fields.
    pure (Map.fromList (zip columns values))
  where
    unquote t = fromMaybe t (T.stripPrefix "'" t >>= T.stripSuffix "'")


-- | The contents of the first @(…)@ group, and whatever follows it. Not
-- paren-balanced: both groups are flat lists of identifiers or literals.
parenGroup :: Text -> Maybe (Text, Text)
parenGroup t = do
    afterOpen <- T.stripPrefix "(" (snd (T.breakOn "(" t))
    let (inner, rest) = T.breakOn ")" afterOpen
    guard (not (T.null rest))
    pure (inner, T.drop 1 rest)


-- | Findings for a changed file that validates a constraint or sets NOT NULL
-- on a table with a registered backfill it does not finalise.
detectUnfinishedDependency :: Map Text (Set Text) -> FilePath -> Text -> IO [Finding]
detectUnfinishedDependency registered path src
    | Map.null registered = pure []
    | otherwise = do
        result <- Pg.parseSql (T.unpack src)
        pure case result of
            Left _ -> []
            Right parsed ->
                let finalised = finalisedNames parsed
                 in -- One finding per open backfill on the table: a table can carry
                    -- one per column, and all must be finished.
                    [ finding (statementRange path src raw) name
                    | raw <- parsed ^.. Pg.stmts . traverse
                    , table <- dependsOn raw
                    , name <- toList (Map.findWithDefault Set.empty table registered)
                    , not (Set.member name finalised)
                    ]


-- | The backfill names a change actually passes to @deferred.ensure_finished@.
-- Read from parsed statements, not the file's text: a textual co-occurrence check
-- would pass on a comment or on a change finalising a different backfill.
-- All string literals in the arguments are collected so named notation still matches.
finalisedNames :: Pg.ParseResult -> Set Text
finalisedNames parsed =
    Set.fromList
        [ literal
        | raw <- parsed ^.. Pg.stmts . traverse
        , call <- toList (view Pg.maybe'callStmt (raw ^. Pg.stmt))
        , let funcCall = call ^. Pg.funccall
        , isEnsureFinished (funcCall ^.. Pg.funcname . traverse . Pg.string . Pg.sval)
        , literal <- concatMap stringLiterals (funcCall ^.. Pg.args . traverse)
        ]
  where
    -- Unqualified is accepted too: a change with a search_path calling it bare
    -- is still finalising.
    isEnsureFinished = \case
        ["ensure_finished"] -> True
        ["deferred", "ensure_finished"] -> True
        _ -> False

    -- A literal, or a named-notation wrapper around one; nothing deeper.
    stringLiterals node =
        directLiteral node
            <> foldMap
                (directLiteral . view Pg.arg)
                (toList (view Pg.maybe'namedArgExpr node))

    directLiteral node = do
        constant <- toList (view Pg.maybe'aConst node)
        string <- toList (view Pg.maybe'sval constant)
        pure (string ^. Pg.sval)


-- | The qualified table a statement depends on the data of, if it is one of the
-- two forms that require the data to already conform.
dependsOn :: Pg.RawStmt -> [Text]
dependsOn raw = do
    stmt <- toList (view Pg.maybe'alterTableStmt (raw ^. Pg.stmt))
    let rel = stmt ^. Pg.relation
        schema = case rel ^. Pg.schemaname of
            "" -> "public"
            s -> s
        table = rel ^. Pg.relname
    guard (any requiresConformingData (stmt ^.. Pg.cmds . traverse))
    pure [iii|#{schema}.#{table}|]


requiresConformingData :: Pg.Node -> Bool
requiresConformingData node =
    case view Pg.maybe'alterTableCmd node of
        Nothing -> False
        Just cmd ->
            cmd ^. Pg.subtype `elem` [Pg.AT_ValidateConstraint, Pg.AT_SetNotNull]


finding :: SourceRange -> Text -> Finding
finding range name =
    Finding
        { range
        , severity = SeverityBlock
        , rule = DeferredUnfinishedBackfill
        , title = "constraint depends on a deferred backfill that this change does not finalise"
        , detail =
            [iii|
            A deferred backfill named `#{name}` is registered for this table.
            Validating the constraint without first calling
            deferred.ensure_finished('#{name}', …) passes in dev and CI, where
            the table is empty, and in production fails on unconverted rows.
            |]
        , suppressedBy = Nothing
        }
