{-# LANGUAGE NoFieldSelectors #-}

-- | Top-level orchestration: discover changed files, run the
-- classifiers, apply suppressions, and emit findings.
--
-- Two passes:
--
--   * /static/ — per changed file, order-independent: the AST
--     lock-class classifier and the indexed-cast detector. This is all
--     the pre-commit hook runs (no Postgres required).
--
--   * /dynamic/ — only when a live Postgres and a baseline change are
--     supplied (CI). The cumulative dynamic pass observes each new change's
--     locks against the correct, progressively-deployed schema (see
--     "Sqint.Cumulative"), and the plan-level cast check runs
--     against the fully-deployed PR schema afterwards.
module Sqint.Run (
    LintConfig (..),
    runLint,
) where

import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as Text.IO
import RIO
import Sqint.Cumulative qualified as Cumulative
import Sqint.DeferredDependency qualified as DeferredDependency
import Sqint.Deploy.Connection (DeploySettings)
import Sqint.Deploy.PlanCheck qualified as PlanCheck
import Sqint.DeployTree (loadDeployTree)
import Sqint.Diagnostic (Finding (..), SourceRange (..), severityExitCode, severityRank)
import Sqint.Domains (DomainSet (..), loadDomainSet)
import Sqint.Output (OutputFormat (..), renderFindings)
import Sqint.PgAst qualified as PgAst
import Sqint.QueryPlan qualified as QueryPlan
import Sqint.Rule (isObservedLock, isPredictedLock)
import Sqint.SetLocal qualified as SetLocal
import Sqint.Suppression qualified as Suppression


data LintConfig = LintConfig
    { paths :: [FilePath]
    , outputFormat :: OutputFormat
    , verbose :: Bool
    -- ^ Show informational and suppressed findings too. Off by default
    -- so the text output stays focused on what needs attention.
    , databaseRoot :: FilePath
    -- ^ The sqitch top directory. The linter reads
    -- @\<databaseRoot\>/deploy/**/*.sql@ for facts about the whole
    -- schema (CHECK-bearing domains, registered backfills), and the
    -- dynamic pass finds each change's deploy script under it.
    , planPath :: FilePath
    -- ^ Path to @sqitch.plan@, used by the cumulative dynamic pass to
    -- order the PR's new changes.
    , baselineChange :: Maybe Text
    -- ^ The target branch's plan head. Everything after it in the plan
    -- is new in this PR. Required for the dynamic pass.
    , postgres :: Maybe DeploySettings
    , deferredBackfills :: Bool
    -- ^ Check that a change finishes the deferred backfills on a table
    -- before it validates a constraint there. Off unless the project
    -- registers backfills in @deferred.migration@.
    }


-- | Run the linter, emit findings, return the exit code. The caller
-- invokes 'System.Exit.exitWith' on it.
runLint :: LintConfig -> IO ExitCode
runLint cfg = do
    -- Read once for the whole run: the deploy tree is large and both of the
    -- facts drawn from it are the same for every changed file.
    deployTree <- loadDeployTree cfg.databaseRoot
    domains <- loadDomainSet deployTree
    let registered
            | cfg.deferredBackfills = DeferredDependency.loadRegisteredBackfills deployTree
            | otherwise = mempty
    static <- concat <$> traverse (staticOne domains.withCheck registered) cfg.paths
    dynamic <- runDynamic cfg
    suppressed <- applySuppressions cfg.paths (domains.parseFailures <> static <> dynamic)
    let final = dedupe suppressed
    Text.IO.putStr (renderFindings cfg.outputFormat cfg.verbose final)
    pure (severityExitCode final)


-- | Static, order-independent classifiers for one changed file.
staticOne :: Set Text -> Map Text (Set Text) -> FilePath -> IO [Finding]
staticOne checkDomains registered path
    | isSnapshotPath path = pure []
    | otherwise = do
        src <- Text.IO.readFile path
        ast <- PgAst.classifySql checkDomains path src
        plan <- QueryPlan.detectIndexedCasts path src
        setLocal <- SetLocal.detectSetLocalAcrossCommit path src
        deferred <- DeferredDependency.detectUnfinishedDependency registered path src
        pure (ast <> plan <> setLocal <> deferred)


-- | The dynamic pass: cumulative lock observation + the plan-level cast
-- check. Skipped entirely unless both a Postgres connection and a
-- baseline change are configured.
runDynamic :: LintConfig -> IO [Finding]
runDynamic cfg = case (cfg.postgres, cfg.baselineChange) of
    (Just settings, Just baseline) -> do
        dyn <- Cumulative.runCumulativeDeploy settings cfg.databaseRoot cfg.planPath baseline
        -- PlanCheck's differential EXPLAIN needs the referenced objects
        -- to exist, so it runs against the fully-deployed PR schema the
        -- cumulative pass just produced.
        pc <- concat <$> traverse (planCheckOne settings) cfg.paths
        pure (dyn <> pc)
    _ -> pure []


planCheckOne :: DeploySettings -> FilePath -> IO [Finding]
planCheckOne settings path
    | isSnapshotPath path = pure []
    | otherwise = PlanCheck.checkFile settings path


-- | Rework snapshot files (@\<name\>@\<tag\>.sql@) are frozen copies of
-- a prior object definition; they are never deployed and linting them
-- only produces noise, so both passes skip them.
isSnapshotPath :: FilePath -> Bool
isSnapshotPath = elem '@'


-- | Apply @-- lint:ignore reason="…"@ annotations across every changed
-- file (path-aware, so an annotation in one file never suppresses a
-- finding in another), and surface malformed annotations.
applySuppressions :: [FilePath] -> [Finding] -> IO [Finding]
applySuppressions paths findings = do
    parsed <- forM paths \path -> do
        msrc <- tryReadFile path
        pure (path, maybe ([], []) (Suppression.parseAnnotations path) msrc)
    let annsByPath = Map.fromList [(path, fst result) | (path, result) <- parsed]
        malformed = concatMap (snd . snd) parsed
        mark f = case Map.lookup f.range.path annsByPath of
            Just anns | [marked] <- Suppression.applyAnnotations anns [f] -> marked
            _ -> f
    pure (malformed <> map mark findings)


tryReadFile :: FilePath -> IO (Maybe Text)
tryReadFile path = either (const Nothing) Just <$> tryAny (Text.IO.readFile path)


-- | Drop a predicted @lock/…@ finding when the dynamic pass observed a
-- lock at the same source location /at least as severe/: the live
-- observation supersedes the static guess, so we keep one line per
-- statement. We keep the static finding when it is stronger than what
-- the dynamic pass saw — e.g. a non-concurrent @CREATE INDEX@ is WARN
-- statically (it blocks writes while building on a populated table),
-- but the dynamic pass, running against the linter's empty tables, only sees
-- a brief lock; the static signal is the one worth surfacing.
dedupe :: [Finding] -> [Finding]
dedupe fs =
    let
        -- Key on the full start position, not just the line: two DDL
        -- statements can share a source line, and the static and dynamic
        -- passes both derive the position from the same statement byte
        -- offset, so the same statement matches while siblings don't.
        loc f = (f.range.path, f.range.startLine, f.range.startCol)
        observedSevAt =
            Map.fromListWith
                max
                [ (loc f, severityRank f.severity)
                | f <- fs
                , isObservedLock f.rule
                ]
        keep f
            | isPredictedLock f.rule =
                case Map.lookup (loc f) observedSevAt of
                    Just observedRank -> observedRank < severityRank f.severity
                    Nothing -> True
            | otherwise = True
     in
        filter keep fs
