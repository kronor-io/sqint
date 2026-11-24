{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Extract static SQL statements embedded in PL/pgSQL function
-- bodies.
--
-- libpg_query's regular SQL parser treats the @$$ … $$@ body of a
-- @CREATE FUNCTION … LANGUAGE plpgsql@ as opaque text, so any cast,
-- DML, or other rule that walks the protobuf AST won't see what's
-- inside. libpg_query does expose a /separate/ PL/pgSQL parser
-- (@pg_query_parse_plpgsql@) which returns a JSON-encoded AST of
-- each function definition; we walk that JSON, pull out every
-- @PLpgSQL_stmt_execsql@ statement, and map its line number back to
-- the outer file.
--
-- Out of scope (deliberately): @PERFORM …@ (@PLpgSQL_stmt_perform@)
-- and @EXECUTE …@ (@PLpgSQL_stmt_dynexecute@). The former wraps an
-- expression we'd then have to re-parse to find a SELECT; the latter
-- builds SQL at runtime from text concatenation and is unparseable
-- statically by definition. Both can be added if we ever grow a rule
-- that cares about them.
--
-- Callers (currently 'Sqint.QueryPlan') re-parse each
-- embedded SQL via 'PgQuery.parseSql' and apply their existing
-- AST-walking rules.
module Sqint.Plpgsql (
    EmbeddedSql (..),
    extractEmbeddedSql,
) where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import PgQuery qualified as Pg
import RIO


-- | One SQL statement extracted from a PL/pgSQL function body.
data EmbeddedSql = EmbeddedSql
    { sql :: Text
    -- ^ Raw SQL text exactly as the plpgsql parser stored it.
    , fileLine :: Int
    -- ^ 1-based line number in the outer source file where this
    -- statement begins. Mapped from the plpgsql JSON's @lineno@
    -- (which is relative to the function body) by counting newlines
    -- between the start of @CREATE FUNCTION@ and the start of the
    -- body in the outer source.
    }


-- | For each @CREATE FUNCTION … LANGUAGE plpgsql@ in the source,
-- return every SQL statement embedded in its body.
--
-- Failures are silent: if libpg_query can't parse the outer SQL or
-- the plpgsql body, we return no findings for the affected
-- statements rather than aborting the whole linter. (The lock-class
-- and top-level cast rules will already have flagged any outer
-- parse failure.)
extractEmbeddedSql :: Text -> IO [EmbeddedSql]
extractEmbeddedSql src = do
    result <- Pg.parseSql (T.unpack src)
    case result of
        Left _ -> pure []
        Right parsed ->
            concat <$> traverse (rawStmtEmbedded src) (parsed ^.. Pg.stmts . traverse)


rawStmtEmbedded :: Text -> Pg.RawStmt -> IO [EmbeddedSql]
rawStmtEmbedded src raw =
    case view Pg.maybe'createFunctionStmt (raw ^. Pg.stmt) of
        Nothing -> pure []
        Just fn
            | not (isPlpgsql fn) -> pure []
            | otherwise -> embeddedFromFunction src raw


-- | A @CREATE FUNCTION@ is plpgsql when one of its 'options' is a
-- @DefElem@ with @defname == "language"@ and an argument whose
-- 'sval' equals @"plpgsql"@.
isPlpgsql :: Pg.CreateFunctionStmt -> Bool
isPlpgsql fn = any langOptionIsPlpgsql (fn ^.. Pg.options . traverse)
  where
    langOptionIsPlpgsql node =
        case view Pg.maybe'defElem node of
            Nothing -> False
            Just de ->
                de
                    ^. Pg.defname
                        == "language"
                        && defElemStringArg de
                        == Just "plpgsql"


-- | A @DefElem@ whose @arg@ is a single @String@ node returns its
-- @sval@; otherwise 'Nothing'.
defElemStringArg :: Pg.DefElem -> Maybe Text
defElemStringArg de = view Pg.sval <$> view Pg.maybe'string (de ^. Pg.arg)


embeddedFromFunction :: Text -> Pg.RawStmt -> IO [EmbeddedSql]
embeddedFromFunction src raw = do
    let stmtText = sliceStmt src raw
        fnStartLine = lineAt src (raw ^. Pg.stmtLocation)
        bodyStartOffsetInStmt = findBodyStartOffset stmtText
        bodyStartLine = fnStartLine + countNewlines (T.take bodyStartOffsetInStmt stmtText)
    parsed <- Pg.parsePlpgsql (T.unpack stmtText)
    case parsed of
        Left _ -> pure []
        Right json ->
            case Aeson.decodeStrict (TE.encodeUtf8 json) of
                Nothing -> pure []
                Just value -> pure (walkJson bodyStartLine value [])


-- | Slice a @RawStmt@'s substring out of the outer source. A
-- @stmt_len@ of 0 means "to the end of the input" per libpg_query's
-- convention.
sliceStmt :: Text -> Pg.RawStmt -> Text
sliceStmt src raw =
    let off = intFromInt32 (raw ^. Pg.stmtLocation)
        len = intFromInt32 (raw ^. Pg.stmtLen)
        rest = T.drop off src
     in if len == 0 then rest else T.take len rest


-- | Find the byte offset within the @CREATE FUNCTION@ substring at
-- which the body text begins — i.e. the character right after the
-- opening @$$@ or @$tag$@. Returns 0 if no dollar-quote marker is
-- found (degenerate; the body is conceptually empty in that case).
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


-- | Recursively walk a parsed plpgsql JSON value, accumulating an
-- 'EmbeddedSql' for every @PLpgSQL_stmt_execsql@ statement.
--
-- We don't need a typed representation of the plpgsql AST — the
-- shape we care about is "anywhere in the tree, an object with this
-- key, containing a 'sqlstmt' / 'query' with the embedded SQL and a
-- 'lineno'". Walking 'Value' generically also keeps us robust to
-- future shape additions.
walkJson :: Int -> Aeson.Value -> [EmbeddedSql] -> [EmbeddedSql]
walkJson bodyStartLine = go
  where
    go (Aeson.Object o) acc =
        let here = mapMaybe (uncurry (extractExecsql bodyStartLine)) (KeyMap.toList o)
            below = foldr go acc (KeyMap.elems o)
         in here <> below
    go (Aeson.Array xs) acc = foldr go acc xs
    go _ acc = acc


-- | An @execsql@ statement wraps an @sqlstmt@ object whose
-- @PLpgSQL_expr@ holds the SQL text in a @query@ field.
extractExecsql :: Int -> Aeson.Key -> Aeson.Value -> Maybe EmbeddedSql
extractExecsql bodyStartLine key value = do
    guard (key == "PLpgSQL_stmt_execsql")
    Aeson.Object stmt <- pure value
    Aeson.Object inner <- KeyMap.lookup "sqlstmt" stmt
    Aeson.Object expr <- KeyMap.lookup "PLpgSQL_expr" inner
    Aeson.String q <- KeyMap.lookup "query" expr
    let lineno = case KeyMap.lookup "lineno" stmt of
            Just (Aeson.Number n) -> max 1 (truncate n)
            _ -> 1
    pure
        EmbeddedSql
            { sql = q
            , fileLine = bodyStartLine + lineno - 1
            }


-- | 1-based outer-file line for a libpg_query byte offset.
lineAt :: Integral n => Text -> n -> Int
lineAt src offset = 1 + countNewlines (T.take (intFromInt32 offset) src)


countNewlines :: Text -> Int
countNewlines = T.count "\n"


intFromInt32 :: Integral n => n -> Int
intFromInt32 n = max 0 (fromIntegral n)
