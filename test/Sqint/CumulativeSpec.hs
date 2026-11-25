{-# LANGUAGE OverloadedStrings #-}

module Sqint.CumulativeSpec (tests) where

import Data.Map.Strict qualified as Map
import Data.String.Interpolate (i)
import Data.Text qualified as T
import RIO
import Sqint.Cumulative (BaselineProblem (..), BrokenFunction (..), FunctionCheck (..), PendingChange (..), brokenFunctionFinding, firstStatementLine, newChangesAfter, recordCheck, trackBrokenFunctions)
import Sqint.Diagnostic (Finding (..), Severity (..), SourceRange (..))
import Sqint.Rule (renderRule)
import Sqitch.Plan qualified as Plan
import Test.Tasty
import Test.Tasty.HUnit


-- | Parse a plan body (header is prepended) or fail the test loudly.
plan :: Text -> Plan.Plan
plan body =
    let src = "%syntax-version=1.0.0\n%project=test\n\n" <> body
     in either (error . T.unpack . Plan.renderParseError) id (Plan.parsePlan src)


changeNamesAfter :: Text -> Text -> Maybe [Text]
changeNamesAfter baseline body =
    either (const Nothing) (Just . map (.name)) (newChangesAfter (plan body) baseline Nothing)


-- | What the pass deploys after the baseline, given how many changes the
-- database has deployed: each change's sqitch target and script.
pendingAfter :: Text -> Maybe Int -> Text -> Either BaselineProblem [(Text, FilePath)]
pendingAfter baseline deployed body =
    map (\c -> (c.target, c.script)) <$> newChangesAfter (plan body) baseline deployed


tag :: Text -> Text
tag name = [i|@#{name} 2024-01-01T00:00:00Z a <a@x> \# tag|] <> "\n"


rework :: Text -> Text -> Text
rework name at = [i|#{name} [#{name}@#{at}] 2024-01-02T00:00:00Z a <a@x> \# reworked #{name}|] <> "\n"


line :: Text -> Text
line name = [i|#{name} 2024-01-01T00:00:00Z a <a@x> \# #{name}|] <> "\n"


tests :: TestTree
tests = testGroup "Cumulative" [newChangesAfterTests, reworkedChangeTests, brokenFunctionTests]


newChangesAfterTests :: TestTree
newChangesAfterTests =
    testGroup
        "Cumulative.newChangesAfter"
        [ testCase "returns the changes strictly after the baseline, in plan order"
            $ changeNamesAfter "b" (line "a" <> line "b" <> line "c" <> line "d")
            @?= Just ["c", "d"]
        , testCase "baseline at the plan head yields no new changes"
            $ changeNamesAfter "c" (line "a" <> line "b" <> line "c")
            @?= Just []
        , testCase "missing baseline yields Nothing (skip, don't redeploy history)"
            $ changeNamesAfter "nope" (line "a" <> line "b")
            @?= Nothing
        , testCase "a rework after the baseline is the new occurrence, in order"
            $
            -- `t` was first created and tagged on the base branch (before
            -- the baseline), then reworked in this PR (after it). Only the
            -- reworked occurrence is new, and it sorts after the baseline.
            changeNamesAfter
                "base"
                ( line "t"
                    <> "@v1 2024-01-01T00:00:00Z a <a@x> # tag\n"
                    <> line "base"
                    <> "t [t@v1] 2024-01-02T00:00:00Z a <a@x> # reworked t\n"
                    <> line "z"
                )
            @?= Just ["t", "z"]
        ]


reworkedChangeTests :: TestTree
reworkedChangeTests =
    testGroup
        "Cumulative.newChangesAfter, reworked changes"
        [ testCase "a change reworked again later deploys from its snapshot, by tag"
            $ pendingAfter
                "a"
                Nothing
                (line "a" <> line "t" <> tag "v1" <> rework "t" "v1" <> tag "v2" <> rework "t" "v2")
            @?= Right
                [ ("t@v1", "deploy/t@v1.sql")
                , ("t@v2", "deploy/t@v2.sql")
                , ("t@HEAD", "deploy/t.sql")
                ]
        , testCase "a change that occurs once is deployed by its bare name"
            $ pendingAfter "a" Nothing (line "a" <> line "b")
            @?= Right [("b", "deploy/b.sql")]
        , testCase "a reworked baseline is placed where the database has deployed to"
            $
            -- The target branch ends with the second occurrence of `t`; this
            -- branch reworks it a third time. The database has deployed the
            -- target branch: three changes.
            pendingAfter "t" (Just 3) (line "a" <> line "t" <> tag "v1" <> rework "t" "v1" <> tag "v2" <> rework "t" "v2")
            @?= Right [("t@HEAD", "deploy/t.sql")]
        , testCase "a reworked baseline the database cannot place is ambiguous, not guessed"
            $ pendingAfter "t" Nothing (line "a" <> line "t" <> tag "v1" <> rework "t" "v1")
            @?= Left (BaselineAmbiguous [5, 7])
        , testCase "a deployed count that lands on another change does not place the baseline"
            $ pendingAfter "t" (Just 1) (line "a" <> line "t" <> tag "v1" <> rework "t" "v1")
            @?= Left (BaselineAmbiguous [5, 7])
        , testCase "a baseline that is not in the plan is missing"
            $ pendingAfter "nope" (Just 1) (line "a")
            @?= Left BaselineMissing
        ]


-- | Run 'trackBrokenFunctions' over a sequence of (change, broken-now)
-- checks, as the cumulative pass does after each deploy.
track :: Map Text Text -> [(Text, Map Text Text)] -> [BrokenFunction]
track known = foldl' step []
  where
    step episodes (name, now) = trackBrokenFunctions known name [i|deploy/#{name}.sql|] 3 now episodes


getter :: Text
getter = "shop.get_transaction_provider_details(shop.orders)"


tooFew :: Text
tooFew = "return type mismatch in function declared to return shop.transaction_provider_details (Final statement returns too few columns.)"


brokenFunctionTests :: TestTree
brokenFunctionTests =
    testGroup
        "Cumulative broken SQL functions"
        [ testCase "a function broken by one change and fixed by a later one is attributed to both"
            $
            -- The usual shape: the migration adds the column, the type rework is a
            -- no-op for the getter, the getter rework fixes it.
            track
                Map.empty
                [ ("migration", Map.singleton getter tooFew)
                , ("type-rework", Map.singleton getter tooFew)
                , ("getter-rework", Map.empty)
                ]
            @?= [ BrokenFunction
                    { signature = getter
                    , message = tooFew
                    , brokenBy = "migration"
                    , script = "deploy/migration.sql"
                    , line = 3
                    , fixedBy = Just "getter-rework"
                    }
                ]
        , testCase "a function still broken after the last change has no fix"
            $ ((.fixedBy) <$> (track Map.empty [("a", Map.singleton getter tooFew), ("b", Map.singleton getter tooFew)]))
            @?= [Nothing]
        , testCase "functions already broken at the baseline are not reported"
            $ track (Map.singleton "settlement.f(bigint)" "excluded_1") [("a", Map.singleton "settlement.f(bigint)" "excluded_1")]
            @?= []
        , testCase "changes that break nothing report nothing"
            $ track Map.empty [("a", Map.empty), ("b", Map.empty)]
            @?= []
        , testCase "a function repaired and then broken again gets a second, open episode"
            $ [(f.brokenBy, f.fixedBy) | f <- track Map.empty [("a", Map.singleton getter tooFew), ("b", Map.empty), ("c", Map.singleton getter tooFew)]]
            @?= [("a", Just "b"), ("c", Nothing)]
        , testCase "a check that fails to run keeps earlier findings and stops later checks" $ do
            let start = FunctionCheck{known = Just Map.empty, episodes = []}
                (afterA, _) = recordCheck "a" "deploy/a.sql" 3 (Right (Map.singleton getter tooFew)) start
                (afterB, skipped) = recordCheck "b" "deploy/b.sql" 3 (Left "statement timeout") afterA
                (afterC, _) = recordCheck "c" "deploy/c.sql" 3 (Right (Map.singleton "x.f()" "broken")) afterB
            [renderRule f.rule | f <- skipped] @?= ["function/check-skipped"]
            afterB.known @?= Nothing
            [(f.signature, f.brokenBy, f.fixedBy) | f <- afterC.episodes] @?= [(getter, "a", Nothing)]
        , testCase "the finding blocks, points at the change's first statement, and names the fix" $ do
            let f =
                    brokenFunctionFinding
                        BrokenFunction
                            { signature = getter
                            , message = tooFew
                            , brokenBy = "migration"
                            , script = "deploy/migration.sql"
                            , line = 3
                            , fixedBy = Just "getter-rework"
                            }
            f.severity @?= SeverityBlock
            renderRule f.rule @?= "function/broken-body"
            (f.range.path, f.range.startLine) @?= ("deploy/migration.sql", 3)
            assertBool "detail names the later change" ("until `getter-rework` deploys" `T.isInfixOf` f.detail)
        , testCase "firstStatementLine skips the sqitch header comment and blank lines"
            $ firstStatementLine "-- Deploy app:x to pg\n\nset lock_timeout to '50ms';\n"
            @?= 3
        , testCase "firstStatementLine of an empty file is 1"
            $ firstStatementLine ""
            @?= 1
        ]
