{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Cumulative, plan-ordered dynamic pass.
--
-- The dynamic pass ("Sqint.Deploy") can only observe the
-- locks a migration takes against a schema that has every /preceding/
-- migration already applied. A pull request is usually a chain of
-- interdependent changes (add a column, backfill it, constrain it, add
-- an FK to a new table…), so observing each change against the bare
-- pre-PR baseline makes the later ones fail with "column/relation does
-- not exist" — false positives, because the migrations are fine, they
-- were just run against the wrong schema state.
--
-- This module fixes that at the source. The caller deploys the target
-- branch's schema up to its plan head (the @baseline@ change). We then
-- walk the PR's /new/ changes in @sqitch.plan@ order and, for each one:
--
--   1. observe its locks against the current (correct) schema state via
--      'Deploy.inspectFile' — a transaction that is rolled back, so the
--      observation never mutates anything; then
--   2. @sqitch deploy --to-change <name>@ it for real, so the next
--      change observes against a schema that includes this one.
--
-- Because the real deploy advances the schema between observations,
-- every change is observed exactly as it will run in production, and
-- non-transactional changes (e.g. @CREATE INDEX CONCURRENTLY@) — which
-- cannot be observed inside a transaction — are still deployed normally
-- so the cumulative state never desyncs.
--
-- After each deploy it also checks that every @language sql@ function
-- still compiles ('Deploy.brokenSqlFunctions'). Sqitch commits each change
-- on its own, so a function left broken by one change and repaired by a
-- later one is broken in production for the time between them, and every
-- call in that window fails. Functions already broken at the baseline are
-- not reported.
module Sqint.Cumulative (
    runCumulativeDeploy,

    -- * Internals exposed for the test-suite
    newChangesAfter,
    PendingChange (..),
    BaselineProblem (..),
    BrokenFunction (..),
    FunctionCheck (..),
    recordCheck,
    trackBrokenFunctions,
    brokenFunctionFinding,
    firstStatementLine,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text qualified as T
import RIO
import RIO.Directory (doesFileExist)
import RIO.FilePath ((</>))
import Sqint.Deploy qualified as Deploy
import Sqint.Deploy.Connection (DeploySettings)
import Sqint.Diagnostic (Finding (..), Severity (..), SourceRange (..))
import Sqint.Rule (Rule (..))
import Sqitch.Plan qualified as Plan
import System.Process (proc, readCreateProcessWithExitCode)


-- | Run the cumulative dynamic pass: observe each new change against
-- the correct cumulative schema, deploying it for real before moving
-- to the next.
runCumulativeDeploy ::
    DeploySettings ->
    -- | The sqitch top directory, which holds @deploy/@.
    FilePath ->
    -- | Path to the PR's @sqitch.plan@.
    FilePath ->
    -- | The baseline change — the target branch's plan head. Everything
    -- after it in the plan is new in this PR.
    Text ->
    IO [Finding]
runCumulativeDeploy settings topDir planPath baseline = do
    planSrc <- readFileUtf8 planPath
    case Plan.parsePlan planSrc of
        Left err -> pure [planParseFinding planPath (Plan.renderParseError err)]
        Right plan -> do
            deployed <- deployedOccurrence settings plan baseline
            case newChangesAfter plan baseline deployed of
                Left BaselineMissing -> pure [baselineMissingFinding planPath baseline]
                Left (BaselineAmbiguous occurrenceLines) -> pure [baselineAmbiguousFinding planPath baseline occurrenceLines]
                Right changes -> observeAndDeploy settings topDir planPath changes


-- | A change the pass deploys: what sqitch calls it, and the script that
-- holds it.
data PendingChange = PendingChange
    { name :: Text
    , target :: Text
    -- ^ The @--to-change@ specifier. A reworked change shares its name
    -- with its other occurrences, so a bare name is ambiguous to sqitch:
    -- an occurrence reworked again later is @name\@tag@, for the tag its
    -- next occurrence was reworked at; the latest occurrence is
    -- @name\@HEAD@; a change that occurs once is its bare name.
    , script :: FilePath
    -- ^ Relative to the sqitch top directory. An occurrence reworked again
    -- later is deployed from its snapshot, @deploy/name\@tag.sql@.
    }
    deriving stock (Eq, Show)


-- | Why the baseline change could not be placed in the plan.
data BaselineProblem
    = -- | The plan has no change of that name.
      BaselineMissing
    | -- | The name occurs more than once and the database did not say
      -- which occurrence it has reached. The lines of each occurrence.
      BaselineAmbiguous [Int]
    deriving stock (Eq, Show)


-- | The changes that come strictly after the @baseline@ change, in
-- plan (deploy) order.
--
-- When the baseline is a reworked change, its name occurs more than once,
-- and the branch may have reworked it yet again after the baseline. The
-- database decides: the baseline is the occurrence at the position the
-- database has deployed up to (the third argument, a count of deployed
-- changes). Without that, an ambiguous baseline is a problem the caller
-- reports, never a guess: guessing the last occurrence would leave
-- nothing to check, and an earlier one would redeploy history.
newChangesAfter :: Plan.Plan -> Text -> Maybe Int -> Either BaselineProblem [PendingChange]
newChangesAfter plan baseline deployed = do
    base <- resolveBaseline plan baseline deployed
    pure (map (pendingChange plan) (Plan.changesAfterLine base.line plan))


resolveBaseline :: Plan.Plan -> Text -> Maybe Int -> Either BaselineProblem (Plan.Located Plan.Change)
resolveBaseline plan baseline deployed = case Plan.occurrences baseline plan of
    [] -> Left BaselineMissing
    [only] -> Right only
    several -> case deployedHead of
        Just c | c.line `elem` map (.line) several -> Right c
        _ -> Left (BaselineAmbiguous (map (.line) several))
  where
    deployedHead = do
        n <- deployed
        guard (n >= 1)
        listToMaybe (drop (n - 1) (Plan.changes plan))


-- | How sqitch names a change at this position, and where its script is.
pendingChange :: Plan.Plan -> Plan.Located Plan.Change -> PendingChange
pendingChange plan c =
    let name = c.value.name
        later = filter (\o -> o.line > c.line) (Plan.occurrences name plan)
        snapshot = case later of
            next : _ -> Plan.reworkTag next.value <|> listToMaybe [t.value.name | t <- Plan.tagsAfterLine c.line plan]
            [] -> Nothing
        current = "deploy" </> T.unpack name <> ".sql"
     in case snapshot of
            Just tag -> PendingChange{name, target = name <> "@" <> tag, script = "deploy" </> T.unpack (name <> "@" <> tag) <> ".sql"}
            Nothing
                | Plan.isReworked name plan -> PendingChange{name, target = name <> "@HEAD", script = current}
                | otherwise -> PendingChange{name, target = name, script = current}


-- | How many changes the database has deployed, when the baseline's name
-- is ambiguous and that count is what places it. 'Nothing' when the name
-- is not ambiguous, or the count could not be read.
deployedOccurrence :: DeploySettings -> Plan.Plan -> Text -> IO (Maybe Int)
deployedOccurrence settings plan baseline
    | length (Plan.occurrences baseline plan) < 2 = pure Nothing
    | otherwise = case Plan.projectName plan of
        Nothing -> pure Nothing
        Just project -> do
            registry <- sqitchRegistry
            either (const Nothing) Just <$> tryAny (Deploy.deployedChangeCount settings registry project)


-- | The registry schema sqitch uses: @engine.pg.registry@, or @sqitch@.
sqitchRegistry :: IO Text
sqitchRegistry = do
    (code, out, _err) <- readCreateProcessWithExitCode (proc "sqitch" ["config", "--get", "engine.pg.registry"]) ""
    pure case code of
        ExitSuccess | not (T.null (T.strip (T.pack out))) -> T.strip (T.pack out)
        _ -> "sqitch"


-- | The observe-then-deploy loop. Stops at the first deploy failure
-- (the schema would be desynced past that point, so later observations
-- would be meaningless) and reports it.
observeAndDeploy :: DeploySettings -> FilePath -> FilePath -> [PendingChange] -> IO [Finding]
observeAndDeploy settings topDir planPath changes = do
    baseline <- tryAny (Deploy.brokenSqlFunctions settings)
    case baseline of
        Left err -> do
            findings <- go FunctionCheck{known = Nothing, episodes = []} [] changes
            pure (functionCheckSkippedFinding planPath (tshow err) : findings)
        Right known -> go FunctionCheck{known = Just known, episodes = []} [] changes
  where
    go functions acc [] = pure (reverse acc <> brokenFindings functions)
    go functions acc (change : rest) = do
        let name = change.name
            script = topDir </> change.script
        exists <- doesFileExist script
        observed <-
            if exists
                then Deploy.inspectFile settings script
                else pure []
        deployed <- sqitchDeployTo change.target
        case deployed of
            Right () -> do
                (functions', skipped) <- checkFunctions functions name script
                go functions' (skipped <> reverse observed <> acc) rest
            Left err ->
                pure (reverse acc <> observed <> [deployFailedFinding script name err] <> brokenFindings functions)

    checkFunctions functions name script
        | isNothing functions.known = pure (functions, [])
        | otherwise = do
            now <- either (Left . tshow) Right <$> tryAny (Deploy.brokenSqlFunctions settings)
            line <- firstStatementLine <$> readFileUtf8OrEmpty script
            pure (recordCheck name script line now functions)

    brokenFindings functions = map brokenFunctionFinding functions.episodes

    readFileUtf8OrEmpty path = fromRight "" <$> tryAny (readFileUtf8 path)


-- | What the SQL function check has established so far.
data FunctionCheck = FunctionCheck
    { known :: Maybe (Map Text Text)
    -- ^ Broken at the baseline; 'Nothing' once a check has failed to run,
    -- which stops further checks.
    , episodes :: [BrokenFunction]
    -- ^ Every time a change in this PR left a function broken, oldest first.
    }


-- | Fold one change's check result into the state. A check that fails to
-- run stops later checks but keeps what earlier checks found, so failing
-- to gather more evidence never hides a broken function already seen.
recordCheck ::
    -- | The change just deployed.
    Text ->
    -- | Its deploy script.
    FilePath ->
    -- | The line findings for this change point at.
    Int ->
    -- | Broken now, or why the check could not run.
    Either Text (Map Text Text) ->
    FunctionCheck ->
    (FunctionCheck, [Finding])
recordCheck name script line result functions = case (functions.known, result) of
    (Nothing, _) -> (functions, [])
    (Just _, Left err) -> (functions{known = Nothing}, [functionCheckSkippedFinding script err])
    (Just known, Right now) -> (functions{episodes = trackBrokenFunctions known name script line now functions.episodes}, [])


-- | A function a change in this PR left broken.
data BrokenFunction = BrokenFunction
    { signature :: Text
    , message :: Text
    , brokenBy :: Text
    , script :: FilePath
    , line :: Int
    , fixedBy :: Maybe Text
    }
    deriving stock (Eq, Show)


-- | Fold one change's check into the episodes so far: an open episode
-- whose function works again is closed as fixed by this change, and a
-- function broken now that was not broken at the baseline and has no
-- open episode starts a new one attributed to this change. A function
-- that is repaired and later broken again therefore gets a second
-- episode, and so a second finding.
trackBrokenFunctions ::
    -- | Broken at the baseline.
    Map Text Text ->
    -- | The change just deployed.
    Text ->
    -- | Its deploy script.
    FilePath ->
    -- | The line findings for this change point at.
    Int ->
    -- | Broken now.
    Map Text Text ->
    [BrokenFunction] ->
    [BrokenFunction]
trackBrokenFunctions known name script line now episodes =
    let isOpen f = isNothing f.fixedBy
        close f
            | isOpen f && not (Map.member f.signature now) = f{fixedBy = Just name}
            | otherwise = f
        stillOpen = Set.fromList [f.signature | f <- episodes, isOpen f, Map.member f.signature now]
        fresh =
            [ BrokenFunction{signature, message = err, brokenBy = name, script, line, fixedBy = Nothing}
            | (signature, err) <- Map.toList (now `Map.difference` known)
            , not (Set.member signature stillOpen)
            ]
     in map close episodes <> fresh


-- | The finding for a function a change left broken.
brokenFunctionFinding :: BrokenFunction -> Finding
brokenFunctionFinding f =
    Finding
        { range = SourceRange{path = f.script, startLine = f.line, startCol = 1, endLine = f.line, endCol = 1}
        , severity = SeverityBlock
        , rule = FunctionBrokenBody
        , title = [i|`#{signature}` is broken after deploying `#{brokenBy}`|]
        , detail = [i|Postgres rejects the current definition of `#{signature}` once `#{brokenBy}` is deployed: #{message}. #{remaining} Sqitch commits each change on its own, so every call to the function fails for as long as it is broken. Redefine the function in the same change that breaks it (for a new column, inside the migration's transaction).|]
        , suppressedBy = Nothing
        }
  where
    signature = f.signature
    brokenBy = f.brokenBy
    message = f.message
    remaining = case f.fixedBy of
        Just later -> [i|It stays broken until `#{later}` deploys, so production sees it broken during the deploy.|] :: Text
        Nothing -> "No later change in this PR fixes it, so it stays broken after the deploy."


-- | The first line of a deploy script that is neither blank nor a comment,
-- so a @-- lint:ignore reason="…"@ placed above the first statement
-- suppresses findings attributed to the whole change. 1 for an empty file.
firstStatementLine :: Text -> Int
firstStatementLine src =
    case [n | (n, l) <- zip [1 ..] (T.lines src), let s = T.strip l, not (T.null s), not ("--" `T.isPrefixOf` s)] of
        (n : _) -> n
        [] -> 1


-- | @sqitch deploy --to-change <target> --verify@ from the working
-- directory, where @sqitch.conf@ is. Connection parameters come from the
-- inherited @PG*@ environment, which the caller sets to the same database
-- as the linter's DSN, so each deploy writes to the registry the baseline
-- deploy populated.
sqitchDeployTo :: Text -> IO (Either Text ())
sqitchDeployTo target = do
    (code, _out, err) <-
        readCreateProcessWithExitCode
            (proc "sqitch" ["deploy", "--to-change", T.unpack target, "--verify"])
            ""
    pure case code of
        ExitSuccess -> Right ()
        ExitFailure _ -> Left (T.strip (T.pack err))


pointFinding :: FilePath -> Severity -> Rule -> Text -> Text -> Finding
pointFinding path severity rule title detail =
    Finding
        { range = SourceRange{path, startLine = 1, startCol = 1, endLine = 1, endCol = 1}
        , severity
        , rule
        , title
        , detail
        , suppressedBy = Nothing
        }


planParseFinding :: FilePath -> Text -> Finding
planParseFinding path err =
    pointFinding
        path
        SeverityInfo
        SetupUnparsablePlan
        "Could not parse sqitch.plan for the cumulative dynamic pass"
        ("Dynamic lock observation was skipped; static checks still applied. " <> err)


baselineMissingFinding :: FilePath -> Text -> Finding
baselineMissingFinding path baseline =
    pointFinding
        path
        SeverityInfo
        SetupBaselineMissing
        "Baseline change not found in sqitch.plan"
        [i|The baseline change `#{baseline}` is not present in the plan, so the cumulative dynamic pass was skipped; static checks still applied.|]


baselineAmbiguousFinding :: FilePath -> Text -> [Int] -> Finding
baselineAmbiguousFinding path baseline occurrenceLines =
    pointFinding
        path
        SeverityInfo
        SetupBaselineAmbiguous
        "Baseline change is reworked, and the database did not say which occurrence it is at"
        [i|`#{baseline}` occurs #{length occurrenceLines} times in the plan (lines #{T.intercalate ", " (map tshow occurrenceLines)}), and the sqitch registry on the database could not be read to tell which occurrence is deployed, so the cumulative dynamic pass was skipped; static checks still applied. Deploy the target branch's plan into the database before running sqint.|]


deployFailedFinding :: FilePath -> Text -> Text -> Finding
deployFailedFinding path name err =
    pointFinding
        path
        SeverityBlock
        DeployFailed
        [i|Could not deploy `#{name}`; later changes were not observed|]
        [i|The cumulative dynamic pass stopped here because `sqitch deploy` failed. Subsequent migrations in this PR were not lock-observed. Error: #{err}|]


functionCheckSkippedFinding :: FilePath -> Text -> Finding
functionCheckSkippedFinding planPath err =
    pointFinding
        planPath
        SeverityWarn
        FunctionCheckSkipped
        "Could not check SQL functions after deploying"
        [i|The check for SQL functions left broken between changes did not run to completion, so this PR was not checked for them. Error: #{err}|]
