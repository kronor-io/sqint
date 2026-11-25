{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | The dynamic pass: deploy each statement and observe its locks.
--
-- Given a connection to a Postgres database whose schema matches the
-- state immediately /before/ the candidate migration, we run each
-- top-level statement in the file inside a savepoint-wrapped
-- transaction on one connection (the @executor@). After each statement
-- a second connection (the @observer@) queries @pg_locks@ for the
-- executor's pid — the locks the statement acquired are still held
-- (transactions release locks only on commit/rollback). A relfilenode
-- snapshot via the executor (which sees its own uncommitted catalog
-- changes) reveals table rewrites within the same transaction.
--
-- We never commit; the transaction is rolled back at the end, leaving
-- the database in its pre-inspection state.
--
-- Implementation note: the executor uses the raw driver connection via
-- 'Sqint.Deploy.Connection.withPqConnection' rather than the Hasql Session
-- API. We need to keep one explicit transaction open across many
-- separate Haskell calls (so locks remain visible to the observer
-- between statements), and Hasql's per-statement extended-query
-- protocol implicitly commits when used at the top level. Raw libpq's
-- 'PQ.exec' uses the simple-query protocol, which leaves transaction
-- state alone.
module Sqint.Deploy (
    inspectFile,
    brokenSqlFunctions,
    deployedChangeCount,

    -- * Internals exposed for the test-suite
    lockFindings,
    computeImpacts,
    TableStat (..),
    RelOid (..),
) where

import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.IO qualified as Text.IO
import Hasql.Connection (Connection)
import PgQuery qualified as Pg
import Pqi qualified as PQ
import RIO
import Sqint.Deploy.Connection (DeploySettings, runSession, withConnection, withPqConnection)
import Sqint.Deploy.Introspector (Pid (..), RelFileNode (..), TableName (..))
import Sqint.Deploy.Observer (PidLock (..), listConnectionLocks)
import Sqint.Diagnostic (Finding (..), Severity (..), SourceRange (..))
import Sqint.Location (statementRange)
import Sqint.Lock (LockClass (..), LockProfile (..), RewriteImpact (..), lockClassName, parseLockMode, severityFor)
import Sqint.Rule (Rule (..))


-- | Inspect a single SQL file. Opens two connections (executor +
-- observer), runs each top-level statement on the executor inside a
-- savepoint, observes locks via the observer, snapshots relfilenodes
-- for rewrite detection.
inspectFile :: DeploySettings -> FilePath -> IO [Finding]
inspectFile settings path = do
    src <- Text.IO.readFile path
    stmts <- splitStatements path src
    case stmts of
        [] -> pure []
        _ ->
            withConnection settings \executor ->
                withConnection settings \observer ->
                    withPqConnection executor \pq -> do
                        execSimple pq "BEGIN"
                        execSimple pq "SET LOCAL lock_timeout = '0'"
                        execSimple pq setLinterStatementTimeout
                        pid <- fetchPid pq
                        findings <- runStatements pq observer pid stmts
                        rollback pq
                        pure findings


-- | A top-level statement extracted from a file.
data Stmt = Stmt
    { range :: SourceRange
    , text :: Text
    }


-- | Split via libpg_query, strip transaction-control statements (the
-- dynamic pass wraps the whole file in its own transaction).
splitStatements :: FilePath -> Text -> IO [Stmt]
splitStatements path src = do
    result <- Pg.parseSql (T.unpack src)
    case result of
        Left _ -> pure []
        Right parsed -> do
            let raws = parsed ^.. Pg.stmts . traverse
                kept =
                    [ stmt
                    | r <- raws
                    , hasContent r src
                    , let stmt@Stmt{text} = stmtFromRaw path src r
                    , not (isTransactionControl text)
                    ]
            pure kept


isTransactionControl :: Text -> Bool
isTransactionControl t =
    let upper = T.toUpper (T.stripStart t)
        startsWith prefix = T.isPrefixOf prefix upper
     in any
            startsWith
            [ "BEGIN"
            , "START TRANSACTION"
            , "COMMIT"
            , "ROLLBACK"
            , "END"
            , "SAVEPOINT"
            , "RELEASE"
            , "SET "
            ]


hasContent :: Pg.RawStmt -> Text -> Bool
hasContent raw src =
    not (T.null (T.strip (extractStmtText src (raw ^. Pg.stmtLocation) (raw ^. Pg.stmtLen))))


stmtFromRaw :: FilePath -> Text -> Pg.RawStmt -> Stmt
stmtFromRaw path src raw =
    let loc = raw ^. Pg.stmtLocation
        len = raw ^. Pg.stmtLen
        text = stripLeadingComments (T.strip (extractStmtText src loc len))
        range = statementRange path src raw
     in Stmt{range, text}


-- | libpg_query's @stmt_location@ points at the start of the statement
-- /including/ preceding comments and whitespace, so the text we slice
-- out always carries a header of @-- …@ lines. Strip them so the
-- 'isTransactionControl' classifier can see the actual SQL keyword
-- start, and so libpq doesn't ever interpret the comments as part of
-- the statement payload.
stripLeadingComments :: Text -> Text
stripLeadingComments t =
    let s = T.stripStart t
     in case T.stripPrefix "--" s of
            Just rest -> stripLeadingComments (T.dropWhile (/= '\n') rest)
            Nothing -> case T.stripPrefix "/*" s of
                Just rest -> case T.breakOn "*/" rest of
                    (_, suffix)
                        | T.null suffix -> s -- unterminated comment; leave as-is
                        | otherwise -> stripLeadingComments (T.drop 2 suffix)
                Nothing -> s


extractStmtText :: Integral n => Text -> n -> n -> Text
extractStmtText src offset len =
    let bytes = encodeUtf8 src
        o = max 0 (fromIntegral offset)
        l = max 0 (fromIntegral len)
        sliced
            | l == 0 = BS8.drop o bytes
            | otherwise = BS8.take l (BS8.drop o bytes)
     in decodeUtf8Lenient sliced


-- | Run each statement with savepoint isolation, accumulating
-- findings. A failure rolls back to the savepoint so subsequent
-- statements can still run.
runStatements ::
    PQ.Connection ->
    Connection ->
    Pid ->
    [Stmt] ->
    IO [Finding]
runStatements executor observer pid stmts = do
    -- One stats snapshot per statement: the "after" snapshot of a
    -- successful statement is the "before" of the next (same
    -- transaction, RELEASE keeps its effects), and a rolled-back
    -- statement discards its sub-transaction stats, so the carried
    -- snapshot stays accurate without re-querying.
    initial <- fetchTableStats executor
    let existing = Set.fromList [stat.relOid | stat <- Map.elems initial]
    go existing initial [] stmts
  where
    go _ _ acc [] = pure (reverse acc)
    go existing statsBefore acc (stmt@Stmt{range} : rest) = do
        before <- runSession observer (listConnectionLocks pid)
        execSimple executor "SAVEPOINT lint_deploy_sp"
        result <- runStatement executor stmt
        case result of
            StmtOk -> do
                after <- runSession observer (listConnectionLocks pid)
                statsAfter <- fetchTableStats executor
                let newLocks =
                        Set.fromList after
                            `Set.difference` Set.fromList before
                    impacts = computeImpacts existing statsBefore statsAfter
                    findings = lockFindings range newLocks impacts
                execSimple executor "RELEASE SAVEPOINT lint_deploy_sp"
                go existing statsAfter (findings <> acc) rest
            StmtNonTransactional -> do
                tryRollbackSavepoint executor
                go existing statsBefore (warnNonTransactional stmt : acc) rest
            StmtError msg -> do
                tryRollbackSavepoint executor
                go existing statsBefore (warnExecutionError stmt msg : acc) rest


tryRollbackSavepoint :: PQ.Connection -> IO ()
tryRollbackSavepoint pq = do
    -- ROLLBACK TO returns commands to "released" state on success; if
    -- the savepoint is gone we silently move on.
    _ <- tryAny do
        execSimple pq "ROLLBACK TO SAVEPOINT lint_deploy_sp"
        execSimple pq "RELEASE SAVEPOINT lint_deploy_sp"
    pure ()


data StmtOutcome
    = StmtOk
    | StmtNonTransactional
    | StmtError Text


-- | Execute the candidate statement on the executor. We classify the
-- failure reason for non-transactional statements so the diagnostic
-- pipeline can WARN-instead-of-BLOCK.
runStatement :: PQ.Connection -> Stmt -> IO StmtOutcome
runStatement executor Stmt{text} = do
    let bytes = encodeUtf8 text
    result <- tryExecSimple executor bytes
    case result of
        Right () -> pure StmtOk
        Left msg ->
            pure
                $ if "cannot run inside a transaction block" `T.isInfixOf` msg
                    then StmtNonTransactional
                    else StmtError msg


warnNonTransactional :: Stmt -> Finding
warnNonTransactional Stmt{range} =
    Finding
        { range
        , severity = SeverityInfo
        , rule = DeployNonTransactional
        , title = "Statement cannot be inspected dynamically (non-transactional DDL)"
        , detail = "Static lock-class classification still applies."
        , suppressedBy = Nothing
        }


-- | A statement the dynamic pass could not execute. After the
-- cumulative deploy lands each preceding change for real, a genuinely
-- correct migration's dependencies always exist, so this is
-- informational ("we couldn't dynamically verify this one"), not a
-- problem with the migration — the static rules still applied.
warnExecutionError :: Stmt -> Text -> Finding
warnExecutionError Stmt{range} msg =
    Finding
        { range
        , severity = SeverityInfo
        , rule = DeployStatementNotObserved
        , title = "The dynamic pass could not execute this statement (static checks still applied)"
        , detail = msg
        , suppressedBy = Nothing
        }


-- | One finding per (table, lock-mode) the statement acquired, with the
-- lock's /impact/ folded in: whether the statement also rewrote the
-- table (relfilenode changed) or scanned it under the lock
-- (transaction-local sequential-scan counter rose). That impact is what
-- decides severity and the SAFE/RISKY reason, so a catalog-only
-- AccessExclusiveLock reads very differently from one held across a
-- full-table scan or rewrite.
lockFindings :: SourceRange -> Set PidLock -> Map TableName RewriteImpact -> [Finding]
lockFindings range newLocks impacts =
    let lockBased =
            [ observedLockFinding range tname cls (Map.findWithDefault NoImpact (TableName tname) impacts)
            | PidLock{table = TableName tname, mode} <- Set.toList newLocks
            , Just cls <- [parseLockMode mode]
            ]
        covered = Set.fromList [t | PidLock{table = t} <- Set.toList newLocks]
        -- A table the statement rewrote/scanned without our observing a
        -- \*new* lock (the lock was taken by an earlier statement in the
        -- same change) still warrants a finding; such operations always
        -- run under AccessExclusiveLock.
        extra =
            [ observedLockFinding range tname AccessExclusive impact
            | (TableName tname, impact) <- Map.toList impacts
            , not (Set.member (TableName tname) covered)
            ]
     in lockBased <> extra


observedLockFinding :: SourceRange -> Text -> LockClass -> RewriteImpact -> Finding
observedLockFinding range tname cls impact =
    Finding
        { range
        , severity = severityFor LockProfile{lockClass = cls, rewriteImpact = impact}
        , rule = LockObserved cls
        , title = lockTitle cls impact tname
        , detail = lockDetail cls impact
        , suppressedBy = Nothing
        }


lockTitle :: LockClass -> RewriteImpact -> Text -> Text
lockTitle cls impact tname =
    let base = "Acquired " <> lockClassName cls <> "Lock on " <> tname
     in case (cls, impact) of
            (AccessExclusive, TableRewrite) -> base <> " and rewrote the table"
            (AccessExclusive, TableScan) -> base <> " while scanning the whole table"
            (AccessExclusive, NoImpact) -> base <> " (catalog-only)"
            _ -> base


lockDetail :: LockClass -> RewriteImpact -> Text
lockDetail AccessExclusive TableRewrite =
    "RISKY: a physical table rewrite runs under AccessExclusiveLock; every transaction touching the table blocks until the rewrite finishes. Use an expand/contract migration instead."
lockDetail AccessExclusive TableScan =
    "RISKY: the whole table is scanned while AccessExclusiveLock is held, so every transaction touching it blocks for the duration of the scan. Split the validation out (e.g. ADD CONSTRAINT ... NOT VALID, then VALIDATE CONSTRAINT, which scans under a weaker lock)."
lockDetail AccessExclusive NoImpact =
    "SAFE: AccessExclusiveLock held only briefly for a catalog-only change (no scan, no rewrite)."
lockDetail _ _ = ""


-- | Per-table impact of a single statement, derived from before/after
-- snapshots: a changed relfilenode means a physical rewrite; otherwise
-- a rise in the transaction's sequential-scan /count/ means the
-- statement scanned the table (e.g. @SET NOT NULL@, @VALIDATE
-- CONSTRAINT@, @ADD CONSTRAINT@ that validates) while holding its lock.
--
-- We count scans, not tuples read, on purpose: the linter's database is
-- schema-only (no rows), so a tuple-read delta would be zero and these
-- scans would look free. The scan-count counter still ticks when the
-- planner executes a sequential scan over an empty relation, so the
-- dangerous "scan under AccessExclusiveLock" shape is caught regardless
-- of data volume. Only non-'NoImpact' tables are returned.
--
-- Tables whose OID is not in @existing@ were created by this file; they
-- are invisible to other transactions until commit, so they are skipped.
computeImpacts :: Set RelOid -> Map TableName TableStat -> Map TableName TableStat -> Map TableName RewriteImpact
computeImpacts existing before after =
    Map.fromList
        [ (tname, impact)
        | (tname, a) <- Map.toList after
        , Set.member a.relOid existing
        , Just b <- [Map.lookup tname before]
        , let impact = classify b a
        , impact /= NoImpact
        ]
  where
    classify b a
        | b.relFileNode /= a.relFileNode = TableRewrite
        | a.seqScans > b.seqScans = TableScan
        | otherwise = NoImpact


-- ---------------------------------------------------------------------
-- libpq helpers (executor connection)
-- ---------------------------------------------------------------------

newtype DeployQueryError = DeployQueryError Text
    deriving stock (Show)


instance Exception DeployQueryError


-- | Run a simple-query statement on the executor. Throws on error.
execSimple :: PQ.Connection -> ByteString -> IO ()
execSimple conn sql = do
    result <- tryExecSimple conn sql
    case result of
        Right () -> pure ()
        Left msg -> throwIO (DeployQueryError (decodeUtf8Lenient sql <> ": " <> msg))


-- | Variant of 'execSimple' that returns a 'Left' on PG-side errors
-- instead of throwing. Used for the candidate statement, whose failure
-- is informational.
tryExecSimple :: PQ.Connection -> ByteString -> IO (Either Text ())
tryExecSimple conn sql = do
    result <- execTuples conn sql
    pure (void result)


-- | Run a simple-query statement and return the 'PQ.Result' for
-- callers that need to read rows. The result is wrapped in 'Either Text'
-- so PG-side errors carry their message instead of an opaque 'Nothing'.
execTuples :: PQ.Connection -> ByteString -> IO (Either Text PQ.Result)
execTuples conn sql = do
    mResult <- PQ.exec conn sql
    case mResult of
        Nothing -> do
            mErr <- PQ.errorMessage conn
            pure (Left (maybe "libpq: no result" decodeUtf8Lenient mErr))
        Just result -> do
            status <- PQ.resultStatus result
            case status of
                PQ.CommandOk -> pure (Right result)
                PQ.TuplesOk -> pure (Right result)
                PQ.EmptyQuery -> pure (Right result)
                _ -> do
                    mErr <- PQ.resultErrorMessage result
                    pure (Left (maybe (tshow status) decodeUtf8Lenient mErr))


-- | Run an exec, throw on error, return rows; the inverse of
-- 'tryExecSimple' for queries we cannot let fail (catalog reads).
execTuplesOrThrow :: Text -> PQ.Connection -> ByteString -> IO PQ.Result
execTuplesOrThrow context conn sql = do
    result <- execTuples conn sql
    case result of
        Right r -> pure r
        Left msg -> throwIO (DeployQueryError (context <> ": " <> msg))


-- | The statement budget the linter gives its own work.
setLinterStatementTimeout :: ByteString
setLinterStatementTimeout = "SET LOCAL statement_timeout = '60s'"


-- | Run one of the linter's own queries under the linter's statement budget,
-- then hand the session back whatever the migration had set.
--
-- The candidate statement runs on this connection and can change
-- @statement_timeout@ for the rest of the transaction, and migrations that
-- take locks routinely do: @select set_config('statement_timeout', '50ms', false)@
-- as a change's first line keeps a blocked @ALTER@ from queueing every query
-- behind it. That 50ms then applies to 'fetchTableStats', which reads
-- @pg_class@ for every relation in the database — 68ms for about 1900
-- relations on a CI runner, 22ms on a developer machine. So the linter cancelled
-- its own snapshot and reported it as the migration timing out, on hosts slow
-- enough to cross the line and not on others.
--
-- Restoring the previous value rather than leaving 60s in place keeps the
-- observation honest: a statement that follows one setting a 50ms budget must
-- still run under 50ms, because that is what production will do.
withLinterStatementTimeout :: PQ.Connection -> IO a -> IO a
withLinterStatementTimeout conn action = do
    saved <- fetchStatementTimeout conn
    execSimple conn setLinterStatementTimeout
    action `finally` restore saved
  where
    -- Best-effort: on the exception path the transaction is abandoned anyway,
    -- and a failure to restore must not mask the original error.
    restore saved = void (tryAny (execSimple conn (assignment saved)))
    assignment saved = BS8.concat ["SET LOCAL statement_timeout = '", escapeLiteral saved, "'"]
    escapeLiteral = BS8.intercalate "''" . BS8.split '\''


fetchStatementTimeout :: PQ.Connection -> IO ByteString
fetchStatementTimeout conn = do
    result <- execTuplesOrThrow "fetchStatementTimeout" conn "select current_setting('statement_timeout')"
    rows <- PQ.ntuples result
    when (rows /= 1)
        $ throwIO (DeployQueryError ("fetchStatementTimeout: expected 1 row, got " <> tshow rows))
    getCellOrThrow "fetchStatementTimeout" result 0 0


rollback :: PQ.Connection -> IO ()
rollback pq = do
    _ <- tryAny (execSimple pq "ROLLBACK")
    pure ()


fetchPid :: PQ.Connection -> IO Pid
fetchPid conn = do
    result <- execTuplesOrThrow "fetchPid" conn "SELECT pg_backend_pid()::text"
    rows <- PQ.ntuples result
    when (rows /= 1) $ throwIO (DeployQueryError ("fetchPid: expected 1 row, got " <> tshow rows))
    bs <- getCellOrThrow "fetchPid" result 0 0
    case readMaybe (BS8.unpack bs) of
        Just n -> pure (Pid n)
        Nothing -> throwIO (DeployQueryError ("fetchPid: not an int: " <> decodeUtf8Lenient bs))


-- | A per-table snapshot the impact diff is computed from: the table's
-- OID (stable across renames), its physical relfilenode (to detect
-- rewrites) and the current transaction's cumulative sequential-scan
-- count for it (to detect scans under the lock, even on the linter's
-- empty tables).
data TableStat = TableStat
    { relOid :: RelOid
    , relFileNode :: RelFileNode
    , seqScans :: Int64
    }


newtype RelOid = RelOid Int64
    deriving stock (Eq, Ord)


fetchTableStats :: PQ.Connection -> IO (Map TableName TableStat)
fetchTableStats conn = withLinterStatementTimeout conn do
    result <- execTuplesOrThrow "fetchTableStats" conn tableStatsSql
    rows <- PQ.ntuples result
    Map.fromList <$> forM (rowIndices rows) \r -> do
        name <- getCellOrThrow "fetchTableStats (relname)" result r 0
        node <- getCellOrThrow "fetchTableStats (filenode)" result r 1
        scan <- getCellOrThrow "fetchTableStats (seq_scan)" result r 2
        oid <- getCellOrThrow "fetchTableStats (oid)" result r 3
        rel <- parseInt64 "filenode" node
        sc <- parseInt64 "seq_scan" scan
        o <- parseInt64 "oid" oid
        pure
            ( TableName (decodeUtf8Lenient name)
            , TableStat{relOid = RelOid o, relFileNode = RelFileNode rel, seqScans = sc}
            )
  where
    parseInt64 :: Text -> ByteString -> IO Int64
    parseInt64 label bs = case readMaybe (BS8.unpack bs) of
        Just n -> pure n
        Nothing ->
            throwIO (DeployQueryError ("fetchTableStats: bad " <> label <> ": " <> decodeUtf8Lenient bs))


-- | Every @language sql@ function whose current definition Postgres now
-- rejects, keyed by signature, with the error it raises.
--
-- Postgres checks a SQL function's body, including that its result matches
-- the declared return type, only when the function is created. A later
-- change to a type or table the body uses is not re-checked, so a function
-- can be left broken until it is next called. Re-running each function's
-- own @pg_get_functiondef@ surfaces that without calling it. Every
-- re-creation is undone in its own subtransaction, and the outer
-- transaction is rolled back.
brokenSqlFunctions :: DeploySettings -> IO (Map Text Text)
brokenSqlFunctions settings =
    withConnection settings \conn ->
        withPqConnection conn \pq -> do
            execSimple pq "BEGIN"
            execSimple pq setLinterStatementTimeout
            execSimple pq sqlFunctionCheckSql
            result <-
                execTuplesOrThrow
                    "brokenSqlFunctions"
                    pq
                    "select signature, error from pg_temp.lint_broken_sql_functions()"
            rows <- PQ.ntuples result
            broken <- forM (rowIndices rows) \r -> do
                signature <- getCellOrThrow "brokenSqlFunctions (signature)" result r 0
                err <- getCellOrThrow "brokenSqlFunctions (error)" result r 1
                pure (decodeUtf8Lenient signature, decodeUtf8Lenient err)
            rollback pq
            pure (Map.fromList broken)


-- | How many changes of the project the sqitch registry records as deployed
-- on the database. Sqitch deploys in plan order and a revert removes the
-- row, so this is how far into the plan the database is, counting each
-- occurrence of a reworked change.
deployedChangeCount ::
    DeploySettings ->
    -- | The registry schema: sqitch's @engine.pg.registry@, @sqitch@ by default.
    Text ->
    -- | The plan's @%project@.
    Text ->
    IO Int
deployedChangeCount settings registry project =
    withConnection settings \conn ->
        withPqConnection conn \pq -> do
            result <-
                execTuplesOrThrow
                    "deployedChangeCount"
                    pq
                    (encodeUtf8 [i|select count(*) from #{quoteIdent registry}.changes where project = #{quoteLiteral project}|])
            count <- getCellOrThrow "deployedChangeCount" result 0 0
            maybe (throwIO (DeployQueryError ("deployedChangeCount: not a number: " <> decodeUtf8Lenient count))) pure (readMaybe (BS8.unpack count))
  where
    quoteIdent name = let escaped = T.replace "\"" "\"\"" name in [i|"#{escaped}"|] :: Text
    quoteLiteral value = let escaped = T.replace "'" "''" value in [i|'#{escaped}'|] :: Text


-- | A temporary function that re-creates every SQL function from its own
-- definition and returns the ones that fail. @KL001@ is a private SQLSTATE
-- used only to roll back each successful re-creation. Extension-owned
-- functions are skipped: they are not ours to keep consistent.
sqlFunctionCheckSql :: ByteString
sqlFunctionCheckSql =
    """
    create function pg_temp.lint_broken_sql_functions()
        returns table (signature text, error text)
        language plpgsql
    as $fn$
    declare
        f record;
        detail text;
    begin
        for f in
            select p.oid
            from pg_catalog.pg_proc p
            join pg_catalog.pg_namespace n on n.oid = p.pronamespace
            where p.prolang = (select oid from pg_catalog.pg_language where lanname = 'sql')
              and p.prokind in ('f', 'p')
              and n.nspname <> 'information_schema'
              and n.nspname !~ '^pg_'
              and not exists (
                  select 1 from pg_catalog.pg_depend d
                  where d.classid = 'pg_catalog.pg_proc'::regclass
                    and d.objid = p.oid
                    and d.deptype = 'e'
              )
        loop
            begin
                execute pg_catalog.pg_get_functiondef(f.oid);
                raise exception using errcode = 'KL001';
            exception
                when sqlstate 'KL001' then
                    null;
                when others then
                    get stacked diagnostics detail = pg_exception_detail;
                    signature := f.oid::regprocedure::text;
                    error := sqlerrm || coalesce(' (' || nullif(detail, '') || ')', '');
                    return next;
            end;
        end loop;
    end;
    $fn$
    """


-- | Read a single cell from a 'PQ.Result' or throw with a helpful
-- context label.
getCellOrThrow :: Text -> PQ.Result -> Int32 -> Int32 -> IO ByteString
getCellOrThrow context result row col = do
    mVal <- PQ.getvalue result row col
    maybe (throwIO (DeployQueryError (context <> ": null cell"))) pure mVal


-- | Per-table relfilenode (for rewrite detection) and the current
-- transaction's sequential-scan count (for scan detection).
--
-- We drive off @pg_class@ and LEFT JOIN @pg_stat_xact_user_tables@:
-- that view only lists tables touched so far this transaction, so a
-- plain join would drop untouched tables and lose their relfilenode
-- baseline. The view's @seq_scan@ is backend-local, in-progress, and
-- ticks the moment a sequential scan executes — before commit and
-- regardless of row count — so a scan-under-lock shows up even on the
-- linter's empty tables. @coalesce@ defaults untouched tables to zero.
tableStatsSql :: ByteString
tableStatsSql =
    """
    select (quote_ident(n.nspname) || '.' || quote_ident(c.relname))::text,
           c.relfilenode::text,
           coalesce(s.seq_scan, 0)::text,
           c.oid::bigint::text
    from pg_catalog.pg_class c
    join pg_catalog.pg_namespace n on n.oid = c.relnamespace
    left join pg_catalog.pg_stat_xact_user_tables s on s.relid = c.oid
    where n.nspname not in ('pg_catalog', 'information_schema', 'pg_toast')
      and n.nspname not like 'pg_temp_%'
      and n.nspname not like 'pg_toast_temp_%'
      and c.relkind in ('r', 'p')
      and c.relfilenode <> 0
    order by 1
    """


rowIndices :: Int32 -> [Int32]
rowIndices total = [0 .. total - 1]
