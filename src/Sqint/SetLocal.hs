{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Flag transaction-scoped settings (@SET LOCAL@, @set_config(…, true)@) in a
-- plpgsql body that also COMMITs: only the first batch keeps them, later ones
-- fall back to cluster defaults. The fix is @set_config(name, value, false)@.
module Sqint.SetLocal (
    detectSetLocalAcrossCommit,
) where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Char (isSpace)
import Data.String.Interpolate (iii)
import Data.Text qualified as T
import PgQuery qualified as Pg
import RIO
import Sqint.Diagnostic (Finding (..), Severity (..), SourceRange (..))
import Sqint.Plpgsql (decodePlpgsqlJson)
import Sqint.Rule (Rule (..))


-- | One transaction-scoped setting found in a body.
data LocalSet = LocalSet
    { statement :: Text
    , bodyLine :: Int
    }


-- | Findings for every plpgsql body mixing a transaction-scoped setting with a COMMIT.
detectSetLocalAcrossCommit :: FilePath -> Text -> IO [Finding]
detectSetLocalAcrossCommit path src = do
    result <- Pg.parseSql (T.unpack src)
    case result of
        Left _ -> pure []
        Right parsed ->
            concat <$> traverse (rawStmt path src) (parsed ^.. Pg.stmts . traverse)


rawStmt :: FilePath -> Text -> Pg.RawStmt -> IO [Finding]
rawStmt path src raw
    | isPlpgsqlFunction raw = analyse path src raw False
    | isDoBlock raw = analyse path src raw True
    | otherwise = pure []


isDoBlock :: Pg.RawStmt -> Bool
isDoBlock raw = isJust (view Pg.maybe'doStmt (raw ^. Pg.stmt))


isPlpgsqlFunction :: Pg.RawStmt -> Bool
isPlpgsqlFunction raw =
    case view Pg.maybe'createFunctionStmt (raw ^. Pg.stmt) of
        Nothing -> False
        Just fn -> any langOptionIsPlpgsql (fn ^.. Pg.options . traverse)
  where
    langOptionIsPlpgsql node =
        case view Pg.maybe'defElem node of
            Nothing -> False
            Just de ->
                de
                    ^. Pg.defname
                        == "language"
                        && (view Pg.sval <$> view Pg.maybe'string (de ^. Pg.arg))
                        == Just "plpgsql"


-- | libpg_query's plpgsql parser takes function definitions, not @DO@ blocks,
-- so a @DO@ body is wrapped in a synthetic function before parsing.
analyse :: FilePath -> Text -> Pg.RawStmt -> Bool -> IO [Finding]
analyse path src raw isDo = do
    let stmtText = sliceStmt src raw
        stmtStartLine = lineAt src (raw ^. Pg.stmtLocation)
        bodyOffset = findBodyStartOffset stmtText
        bodyStartLine = stmtStartLine + countNewlines (T.take bodyOffset stmtText)
        toParse
            | isDo = wrapDoBlock stmtText
            | otherwise = stmtText
    parsed <- Pg.parsePlpgsql (T.unpack toParse)
    case parsed of
        Left _ -> pure []
        Right json ->
            case decodePlpgsqlJson json of
                Nothing -> pure []
                Just value -> do
                    let locals = collectLocalSets value []
                        commits = hasCommit value
                    pure
                        [ finding path (bodyStartLine + l.bodyLine - 1) l.statement
                        | commits
                        , l <- locals
                        ]


-- | Wrap @do $$ … $$;@ as a function definition the plpgsql parser accepts.
wrapDoBlock :: Text -> Text
wrapDoBlock stmtText =
    let body = T.drop (findBodyStartOffset stmtText - 2) stmtText
        withoutTrailer = T.dropWhileEnd (\c -> c == ';' || isSpace c) body
     in [iii|create function __lint_do_block() returns void as #{withoutTrailer} language plpgsql;|]


-- | Does this body contain a @COMMIT@ or @ROLLBACK@?
hasCommit :: Aeson.Value -> Bool
hasCommit = go
  where
    go (Aeson.Object o) =
        any isTxnKey (KeyMap.keys o) || any go (KeyMap.elems o)
    go (Aeson.Array xs) = any go xs
    go _ = False

    isTxnKey k = k == "PLpgSQL_stmt_commit" || k == "PLpgSQL_stmt_rollback"


-- | Every transaction-scoped setting in the body (execsql or @PERFORM@).
collectLocalSets :: Aeson.Value -> [LocalSet] -> [LocalSet]
collectLocalSets = go
  where
    go (Aeson.Object o) acc =
        let here = mapMaybe (uncurry extract) (KeyMap.toList o)
            below = foldr go acc (KeyMap.elems o)
         in here <> below
    go (Aeson.Array xs) acc = foldr go acc xs
    go _ acc = acc

    extract key value = do
        guard (key == "PLpgSQL_stmt_execsql" || key == "PLpgSQL_stmt_perform")
        Aeson.Object stmt <- pure value
        query <- queryOf stmt
        guard (isTransactionScoped query)
        let lineno = case KeyMap.lookup "lineno" stmt of
                Just (Aeson.Number n) -> max 1 (truncate n)
                _ -> 1
        pure LocalSet{statement = T.strip query, bodyLine = lineno}

    queryOf stmt = do
        Aeson.Object inner <-
            KeyMap.lookup "sqlstmt" stmt <|> KeyMap.lookup "expr" stmt
        Aeson.Object expr <- KeyMap.lookup "PLpgSQL_expr" inner
        Aeson.String q <- KeyMap.lookup "query" expr
        pure q


-- | @SET LOCAL@, or @set_config@ with a @true@ third argument; whitespace is
-- normalised first so the match does not depend on formatting.
isTransactionScoped :: Text -> Bool
isTransactionScoped query =
    ("set local " `T.isPrefixOf` normalised)
        || ("set_config(" `T.isInfixOf` normalised && ",true)" `T.isInfixOf` squashed)
  where
    normalised = T.strip (T.toLower (T.unwords (T.words query)))
    squashed = T.filter (not . isSpace) normalised


finding :: FilePath -> Int -> Text -> Finding
finding path line statement =
    Finding
        { range =
            SourceRange
                { path
                , startLine = line
                , startCol = 1
                , endLine = line
                , endCol = 1
                }
        , severity = SeverityBlock
        , rule = TxnSetLocalAcrossCommit
        , title = "transaction-scoped setting in a block that COMMITs"
        , detail =
            [iii|
            `#{statement}` is scoped to the current transaction, and this block
            COMMITs. Only the first iteration runs with it; every one after
            that falls back to the cluster default (for lock_timeout, 0 --
            wait forever). Use session scope instead:
            set_config(name, value, false).
            |]
        , suppressedBy = Nothing
        }


sliceStmt :: Text -> Pg.RawStmt -> Text
sliceStmt src raw =
    let off = intFromInt32 (raw ^. Pg.stmtLocation)
        len = intFromInt32 (raw ^. Pg.stmtLen)
        rest = T.drop off src
     in if len == 0 then rest else T.take len rest


findBodyStartOffset :: Text -> Int
findBodyStartOffset stmtText =
    case T.breakOn "$" stmtText of
        (_, rest)
            | T.null rest -> 0
            | otherwise ->
                let afterTag = T.dropWhile (/= '$') (T.drop 1 rest)
                 in if T.null afterTag
                        then 0
                        else T.length stmtText - T.length afterTag + 1


lineAt :: Integral n => Text -> n -> Int
lineAt src offset = 1 + countNewlines (T.take (intFromInt32 offset) src)


countNewlines :: Text -> Int
countNewlines = T.count "\n"


intFromInt32 :: Integral n => n -> Int
intFromInt32 n = max 0 (fromIntegral n)
