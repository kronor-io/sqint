{-# LANGUAGE OverloadedStrings #-}

module Sqint.PlanCheckSpec (tests) where

import RIO
import Sqint.Deploy.PlanCheck (Candidate (..), Plan (..), diffPlans, stripPartitionSuffix)
import Sqint.Diagnostic (Finding (..), Severity (..), SourceRange (..))
import Test.Tasty
import Test.Tasty.HUnit


tests :: TestTree
tests =
    testGroup
        "PlanCheck"
        [ stripPartitionSuffixTests
        , diffPlansTests
        ]


-- ---------------------------------------------------------------------
-- stripPartitionSuffix
-- ---------------------------------------------------------------------

stripPartitionSuffixTests :: TestTree
stripPartitionSuffixTests =
    testGroup
        "stripPartitionSuffix"
        [ testCase "strips a _p<digits> partition suffix"
            $ stripPartitionSuffix "checkout_session_p20260201"
            @?= "checkout_session"
        , testCase "strips _default"
            $ stripPartitionSuffix "checkout_session_default"
            @?= "checkout_session"
        , testCase "leaves an unsuffixed relation alone"
            $ stripPartitionSuffix "shop.orders"
            @?= "shop.orders"
        , testCase "does not strip _p followed by non-digits"
            $ stripPartitionSuffix "table_partial"
            @?= "table_partial"
        , testCase "preserves schema-qualified names"
            $ stripPartitionSuffix "shop.checkout_session_p20260201"
            @?= "shop.checkout_session"
        ]


-- ---------------------------------------------------------------------
-- diffPlans
-- ---------------------------------------------------------------------

dummyCandidate :: Candidate
dummyCandidate =
    Candidate
        { sql = "select 1;"
        , range = SourceRange{path = "test.sql", startLine = 1, startCol = 1, endLine = 1, endCol = 1}
        }


seqScanOn :: Text -> Plan
seqScanOn rel = Plan{nodeType = "Seq Scan", relationName = Just rel, indexName = Nothing, plans = []}


indexScanOn :: Text -> Plan
indexScanOn rel =
    Plan{nodeType = "Index Scan", relationName = Just rel, indexName = Just (rel <> "_pkey"), plans = []}


-- Wrap a list of leaf scans under an Append-like parent so we can test
-- multi-scan plans without inventing more node-type strings.
container :: [Plan] -> Plan
container children = Plan{nodeType = "Append", relationName = Nothing, indexName = Nothing, plans = children}


diffPlansTests :: TestTree
diffPlansTests =
    testGroup
        "diffPlans"
        [ testCase "Seq Scan becoming Index Scan emits one BLOCK" $ do
            let castPlan = seqScanOn "checkout_session"
                noCastPlan = indexScanOn "checkout_session"
            map (.severity) (diffPlans dummyCandidate castPlan noCastPlan) @?= [SeverityBlock]
        , testCase "matching Seq Scans on both sides do not flag" $ do
            let castPlan = seqScanOn "checkout_session"
                noCastPlan = seqScanOn "checkout_session"
            diffPlans dummyCandidate castPlan noCastPlan @?= []
        , testCase "matching Index Scans on both sides do not flag" $ do
            let castPlan = indexScanOn "checkout_session"
                noCastPlan = indexScanOn "checkout_session"
            diffPlans dummyCandidate castPlan noCastPlan @?= []
        , testCase "self-join: cast has 2 Seq Scans, no-cast has 1 Seq + 1 Index — flag" $ do
            let castPlan = container [seqScanOn "orders", seqScanOn "orders"]
                noCastPlan = container [seqScanOn "orders", indexScanOn "orders"]
            map (.severity) (diffPlans dummyCandidate castPlan noCastPlan) @?= [SeverityBlock]
        , testCase "self-join: cast and no-cast both have 2 Seq Scans — do not flag" $ do
            let castPlan = container [seqScanOn "orders", seqScanOn "orders"]
                noCastPlan = container [seqScanOn "orders", seqScanOn "orders"]
            diffPlans dummyCandidate castPlan noCastPlan @?= []
        , testCase "partitioned: cast has Seq Scans on partitions, no-cast has Index Scans — flag" $ do
            let castPlan =
                    container
                        [seqScanOn "checkout_session_p20260201", seqScanOn "checkout_session_p20260301"]
                noCastPlan =
                    container
                        [indexScanOn "checkout_session_p20260201", indexScanOn "checkout_session_p20260301"]
                findings = diffPlans dummyCandidate castPlan noCastPlan
            map (.severity) findings @?= [SeverityBlock]
            -- the title rolls partitions up to a single root via stripPartitionSuffix
            map (.title) findings @?= ["Cast on a column forces a sequential scan on checkout_session"]
        , testCase "multi-relation: only the regressed relation is named" $ do
            let castPlan = container [seqScanOn "orders", seqScanOn "customer"]
                noCastPlan = container [indexScanOn "orders", seqScanOn "customer"]
                findings = diffPlans dummyCandidate castPlan noCastPlan
            map (.title) findings @?= ["Cast on a column forces a sequential scan on orders"]
        ]
