{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | DDL lock-class detection via @libpg_query@.
--
-- Walks the protobuf parse tree returned by @pg-query@ and classifies
-- each top-level statement by 'LockProfile' (lock class + rewrite
-- impact). Severity comes from the central policy in
-- "Sqint.Lock".
module Sqint.PgAst (
    classifySql,
) where

import Data.Set qualified as Set
import Data.Text qualified as T
import PgQuery qualified as Pg
import RIO
import Sqint.Diagnostic (Finding (..), Severity (..), SourceRange (..))
import Sqint.Location (statementRange)
import Sqint.Lock (LockClass (..), LockProfile (..), RewriteImpact (..), severityFor)
import Sqint.Rule (Rule (..))


classifySql ::
    -- | Names of user-defined domains that carry a CHECK constraint —
    -- ADD COLUMN with one of these types forces a table scan under
    -- AccessExclusiveLock. Pass 'Set.empty' to disable the rule.
    Set Text ->
    -- | Source path (used for diagnostic locations).
    FilePath ->
    -- | SQL source text.
    Text ->
    IO [Finding]
classifySql checkDomains path src = do
    result <- Pg.parseSql (T.unpack src)
    case result of
        Left err -> pure [parseFailure path err]
        Right parsed ->
            pure
                $ concatMap
                    (classifyRawStmt checkDomains path src)
                    (parsed ^.. Pg.stmts . traverse)


parseFailure :: FilePath -> String -> Finding
parseFailure path err =
    Finding
        { range = SourceRange{path, startLine = 1, startCol = 1, endLine = 1, endCol = 1}
        , severity = SeverityWarn
        , rule = SetupUnparsableSql
        , title = "Could not parse SQL"
        , detail = "libpg_query: " <> T.pack err
        , suppressedBy = Nothing
        }


classifyRawStmt :: Set Text -> FilePath -> Text -> Pg.RawStmt -> [Finding]
classifyRawStmt checkDomains path src raw =
    let node = raw ^. Pg.stmt
        range = statementRange path src raw
     in catMaybes
            [ alterTableFinding checkDomains range node
            , alterDomainFinding range node
            , indexFinding range node
            , dropIndexFinding range node
            ]


-- | An ALTER TABLE statement is a sequence of subcommands. We emit
-- one finding for the first subcommand of concern; subsequent ones
-- would have the same source range anyway.
alterTableFinding :: Set Text -> SourceRange -> Pg.Node -> Maybe Finding
alterTableFinding checkDomains range node = do
    stmt <- view Pg.maybe'alterTableStmt node
    profile <- listToMaybe (mapMaybe (alterTableCmdProfile checkDomains) (stmt ^.. Pg.cmds . traverse))
    pure (toFinding range profile)


-- | Decide the lock profile for a single ALTER TABLE subcommand
-- (extracted from the surrounding 'Pg.Node' wrapper).
alterTableCmdProfile :: Set Text -> Pg.Node -> Maybe (Rule, Text, LockProfile)
alterTableCmdProfile checkDomains node = do
    cmd <- view Pg.maybe'alterTableCmd node
    case cmd ^. Pg.subtype of
        Pg.AT_AddColumn -> Just (addColumnProfile checkDomains (cmd ^. Pg.def))
        Pg.AT_AddConstraint -> Just (addConstraintProfile (cmd ^. Pg.def))
        Pg.AT_AlterColumnType ->
            Just
                ( LockAlterColumnType
                , "ALTER COLUMN TYPE — table rewrite under AccessExclusiveLock unless binary-coercible"
                , LockProfile{lockClass = AccessExclusive, rewriteImpact = TableRewrite}
                )
        _ -> Nothing


addColumnProfile :: Set Text -> Pg.Node -> (Rule, Text, LockProfile)
addColumnProfile checkDomains defNode =
    let constraintTypes = columnDefConstraintTypes defNode
        hasDefault =
            Pg.CONSTR_DEFAULT
                `elem` constraintTypes
                || maybe False (isJust . view Pg.maybe'rawDefault) (view Pg.maybe'columnDef defNode)
        notNull =
            Pg.CONSTR_NOTNULL
                `elem` constraintTypes
                || maybe False (view Pg.isNotNull) (view Pg.maybe'columnDef defNode)
        domainMatch = columnTypeIsCheckDomain checkDomains defNode
        rewrite
            | notNull && not hasDefault = TableScan
            | domainMatch = TableScan
            | otherwise = NoImpact
        title' = case (notNull && not hasDefault, domainMatch) of
            (True, _) ->
                "ADD COLUMN ... NOT NULL without DEFAULT — every row must satisfy the constraint immediately"
            (False, True) ->
                "ADD COLUMN of a CHECK-bearing domain — every existing row is validated under AccessExclusiveLock"
            (False, False) ->
                "ADD COLUMN — quick catalog-only change"
     in (LockAddColumn, title', LockProfile{lockClass = AccessExclusive, rewriteImpact = rewrite})


-- | The declared type name of a column (lower-cased, dotted for
-- qualified names). Returns 'Nothing' if the def node doesn't carry
-- a ColumnDef.
columnTypeName :: Pg.Node -> Maybe Text
columnTypeName defNode = do
    col <- view Pg.maybe'columnDef defNode
    typ <- view Pg.maybe'typeName col
    case typ ^.. Pg.names . traverse . Pg.string . Pg.sval of
        [] -> Nothing
        parts -> Just (T.toLower (T.intercalate "." parts))


columnTypeIsCheckDomain :: Set Text -> Pg.Node -> Bool
columnTypeIsCheckDomain checkDomains defNode =
    maybe False (`Set.member` checkDomains) (columnTypeName defNode)


-- | The PostgreSQL parser puts inline @NOT NULL@, @DEFAULT …@, @CHECK
-- …@, etc. into 'Pg.columnDef'\'s @constraints@ array, not into the
-- top-level boolean flags. Pull out the constraint kinds we care about.
columnDefConstraintTypes :: Pg.Node -> [Pg.ConstrType]
columnDefConstraintTypes defNode =
    case view Pg.maybe'columnDef defNode of
        Nothing -> []
        Just col ->
            mapMaybe
                (fmap (view Pg.contype) . view Pg.maybe'constraint)
                (col ^.. Pg.constraints . traverse)


addConstraintProfile :: Pg.Node -> (Rule, Text, LockProfile)
addConstraintProfile defNode =
    let mConstraint = view Pg.maybe'constraint defNode
        ct = maybe Pg.CONSTR_TYPE_UNDEFINED (view Pg.contype) mConstraint
        skipValidation = maybe False (view Pg.skipValidation) mConstraint
        rewrite
            | ct `elem` [Pg.CONSTR_CHECK, Pg.CONSTR_FOREIGN] =
                if skipValidation then NoImpact else TableScan
            | otherwise = NoImpact
        title' = case ct of
            Pg.CONSTR_CHECK ->
                if skipValidation
                    then "ADD CONSTRAINT CHECK ... NOT VALID — existing rows are not re-checked"
                    else "ADD CONSTRAINT CHECK — scans every row under AccessExclusiveLock"
            Pg.CONSTR_FOREIGN ->
                if skipValidation
                    then "ADD CONSTRAINT FOREIGN KEY ... NOT VALID — existing rows are not re-checked"
                    else "ADD CONSTRAINT FOREIGN KEY — scans every row under AccessExclusiveLock"
            _ -> "ADD CONSTRAINT — review for table scan"
     in (LockAddConstraint, title', LockProfile{lockClass = AccessExclusive, rewriteImpact = rewrite})


-- | @ALTER DOMAIN ADD CONSTRAINT@ scans every column of every table
-- that uses the domain. @NOT VALID@ downgrades that.
alterDomainFinding :: SourceRange -> Pg.Node -> Maybe Finding
alterDomainFinding range node = do
    stmt <- view Pg.maybe'alterDomainStmt node
    -- subtype "C" identifies the ADD CONSTRAINT form per PostgreSQL's
    -- parser; see src/include/nodes/parsenodes.h.
    guard (stmt ^. Pg.subtype == "C")
    let mConstraint = view Pg.maybe'constraint (stmt ^. Pg.def)
        skipValidation = maybe False (view Pg.skipValidation) mConstraint
        rewrite = if skipValidation then NoImpact else TableScan
        title' =
            if skipValidation
                then "ALTER DOMAIN ADD CONSTRAINT ... NOT VALID — tables are not re-scanned"
                else "ALTER DOMAIN ADD CONSTRAINT — scans every column of every table using the domain"
    pure
        ( toFinding
            range
            ( LockAlterDomain
            , title'
            , LockProfile{lockClass = AccessExclusive, rewriteImpact = rewrite}
            )
        )


-- | @CREATE INDEX@ holds Share lock (blocks writes) and scans the
-- table. @CONCURRENTLY@ downgrades both.
indexFinding :: SourceRange -> Pg.Node -> Maybe Finding
indexFinding range node = do
    stmt <- view Pg.maybe'indexStmt node
    let concurrent = stmt ^. Pg.concurrent
        profile =
            if concurrent
                then LockProfile{lockClass = ShareUpdateExclusive, rewriteImpact = TableScan}
                else LockProfile{lockClass = Share, rewriteImpact = TableScan}
        title' =
            if concurrent
                then "CREATE INDEX CONCURRENTLY — concurrent with DML"
                else "CREATE INDEX (non-concurrent) — Share lock blocks writes for the duration of the build"
    pure (toFinding range (LockCreateIndex, title', profile))


-- | @DROP INDEX@ briefly holds AccessExclusiveLock on the table.
-- @CONCURRENTLY@ downgrades to ShareUpdateExclusive.
dropIndexFinding :: SourceRange -> Pg.Node -> Maybe Finding
dropIndexFinding range node = do
    stmt <- view Pg.maybe'dropStmt node
    guard (stmt ^. Pg.removeType == Pg.OBJECT_INDEX)
    let concurrent = stmt ^. Pg.concurrent
        profile =
            if concurrent
                then LockProfile{lockClass = ShareUpdateExclusive, rewriteImpact = NoImpact}
                else LockProfile{lockClass = AccessExclusive, rewriteImpact = NoImpact}
        title' =
            if concurrent
                then "DROP INDEX CONCURRENTLY — concurrent with DML"
                else "DROP INDEX (non-concurrent) — AccessExclusiveLock; blocks every transaction touching the table"
    pure (toFinding range (LockDropIndex, title', profile))


toFinding :: SourceRange -> (Rule, Text, LockProfile) -> Finding
toFinding range (rule, title, profile) =
    Finding
        { range
        , severity = severityFor profile
        , rule
        , title
        , detail = ""
        , suppressedBy = Nothing
        }
