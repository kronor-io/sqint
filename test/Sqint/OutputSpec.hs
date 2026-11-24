{-# LANGUAGE OverloadedStrings #-}

module Sqint.OutputSpec (tests) where

import Data.Text qualified as T
import RIO
import Sqint.Diagnostic (Finding (..), Severity (..), SourceRange (..))
import Sqint.Output (OutputFormat (..), renderFindings)
import Sqint.Rule (Rule (..), renderRule)
import Test.Tasty
import Test.Tasty.HUnit


finding :: Severity -> Rule -> Maybe Text -> Finding
finding severity rule suppressedBy =
    Finding
        { range = SourceRange{path = "f.sql", startLine = 1, startCol = 1, endLine = 1, endCol = 1}
        , severity
        , rule
        , title = renderRule rule <> " title"
        , detail = ""
        , suppressedBy
        }


sample :: [Finding]
sample =
    [ finding SeverityBlock LockAddColumn Nothing
    , finding SeverityWarn TxnSetLocalAcrossCommit Nothing
    , finding SeverityInfo DeployNonTransactional Nothing
    , finding SeverityWarn IndexCastOnColumn (Just "acknowledged")
    ]


render :: Bool -> Text
render verbose = renderFindings OutputText verbose sample


tests :: TestTree
tests =
    testGroup
        "Output.renderFindings (text)"
        [ testCase "default shows only un-suppressed BLOCK/WARN" $ do
            let out = render False
            assertBool "shows BLOCK" ("lock/add-column" `T.isInfixOf` out)
            assertBool "shows WARN" ("txn/set-local-across-commit" `T.isInfixOf` out)
            assertBool "hides INFO" (not ("deploy/non-transactional" `T.isInfixOf` out))
            assertBool "hides suppressed" (not ("index/cast-on-column" `T.isInfixOf` out))
        , testCase "default footer counts the hidden findings"
            $ assertBool
                "footer mentions 2 hidden"
                ("2 informational or suppressed findings hidden" `T.isInfixOf` render False)
        , testCase "verbose shows everything and no footer" $ do
            let out = render True
            assertBool "shows INFO" ("deploy/non-transactional" `T.isInfixOf` out)
            assertBool "shows suppressed" ("index/cast-on-column" `T.isInfixOf` out)
            assertBool "no hidden footer" (not ("hidden — pass --verbose" `T.isInfixOf` out))
        , testCase "no footer when nothing is hidden" $ do
            let out = renderFindings OutputText False [finding SeverityBlock DeployFailed Nothing]
            assertBool "shows the block" ("deploy/failed" `T.isInfixOf` out)
            assertBool "no footer" (not ("hidden" `T.isInfixOf` out))
        ]
