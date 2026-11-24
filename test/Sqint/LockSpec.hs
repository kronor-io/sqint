module Sqint.LockSpec (tests) where

import RIO
import Sqint.Diagnostic (Severity (..))
import Sqint.Lock (LockClass (..), LockProfile (..), RewriteImpact (..), lockClassName, parseLockMode, severityFor)
import Test.Tasty
import Test.Tasty.HUnit


tests :: TestTree
tests =
    testGroup
        "Lock"
        [ testCase "AccessExclusive + TableScan blocks"
            $ severityFor (LockProfile AccessExclusive TableScan)
            @?= SeverityBlock
        , testCase "AccessExclusive + TableRewrite blocks"
            $ severityFor (LockProfile AccessExclusive TableRewrite)
            @?= SeverityBlock
        , testCase "AccessExclusive + NoImpact is info (catalog-only, held briefly)"
            $ severityFor (LockProfile AccessExclusive NoImpact)
            @?= SeverityInfo
        , testCase "Share + TableScan warns"
            $ severityFor (LockProfile Share TableScan)
            @?= SeverityWarn
        , testCase "ShareUpdateExclusive + anything is info"
            $ severityFor (LockProfile ShareUpdateExclusive TableScan)
            @?= SeverityInfo
        , testCase "RowExclusive is info"
            $ severityFor (LockProfile RowExclusive NoImpact)
            @?= SeverityInfo
        , testGroup
            "parseLockMode covers every relation-level lock mode"
            [ testCase mode
                $ parseLockMode (fromString mode)
                @?= Just expected
            | (mode, expected) <-
                [ ("AccessShareLock", AccessShare)
                , ("RowShareLock", RowShare)
                , ("RowExclusiveLock", RowExclusive)
                , ("ShareUpdateExclusiveLock", ShareUpdateExclusive)
                , ("ShareLock", Share)
                , ("ShareRowExclusiveLock", ShareRowExclusive)
                , ("ExclusiveLock", Exclusive)
                , ("AccessExclusiveLock", AccessExclusive)
                ]
            ]
        , testCase "parseLockMode rejects unknown modes"
            $ parseLockMode "NoSuchLock"
            @?= Nothing
        , testCase "lockClassName for the most severe class is \"AccessExclusive\""
            $ lockClassName AccessExclusive
            @?= "AccessExclusive"
        ]
