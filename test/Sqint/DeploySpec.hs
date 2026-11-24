{-# LANGUAGE OverloadedStrings #-}

module Sqint.DeploySpec (tests) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import RIO
import Sqint.Deploy (RelOid (..), TableStat (..), computeImpacts, lockFindings)
import Sqint.Deploy.Connection (parseSettings)
import Sqint.Deploy.Introspector (RelFileNode (..), TableName (..))
import Sqint.Deploy.Observer (PidLock (..))
import Sqint.Diagnostic (Finding (..), Severity (..), SourceRange (..))
import Sqint.Rule (renderRule)
import Test.Tasty
import Test.Tasty.HUnit


tests :: TestTree
tests =
    testGroup
        "Deploy"
        [ testGroup
            -- `parseSettings` defers syntactic validation to libpq, so
            -- the tests pin the contract we *do* enforce here: empty
            -- in → Left, anything non-empty → Right.
            "Connection.parseSettings (non-empty pass-through; format deferred to libpq)"
            [ testCase "accepts a postgres:// URI shape"
                $ isRight (parseSettings "postgres://user:pw@localhost:5432/dbname")
                @?= True
            , testCase "accepts a libpq key=value DSN shape"
                $ isRight (parseSettings "host=localhost user=app dbname=app")
                @?= True
            , testCase "accepts arbitrary non-empty text (libpq will reject at acquire time)"
                $ isRight (parseSettings "not-really-a-dsn")
                @?= True
            , testCase "rejects an empty string"
                $ isLeft (parseSettings "")
                @?= True
            ]
        , lockFindingsTests
        ]


-- The inputs mirror what the dynamic pass observes for `create index`: the heap
-- scan raises seq_scan, and the observer only lists locks on committed tables.
lockFindingsTests :: TestTree
lockFindingsTests =
    testGroup
        "computeImpacts + lockFindings"
        [ testCase "create index on a table created earlier in the same change is not reported"
            $ findings Set.empty (scanned newTable newOid)
            @?= []
        , testCase "create index on a pre-existing table is reported"
            $ findings (Set.singleton (PidLock oldTable "ShareLock")) (scanned oldTable oldOid)
            @?= [("lock/observed-share", SeverityWarn)]
        , testCase "a scan of a pre-existing table locked by an earlier statement is still a BLOCK"
            $ findings Set.empty (scanned oldTable oldOid)
            @?= [("lock/observed-access-exclusive", SeverityBlock)]
        , testCase "a scan of a pre-existing table renamed earlier in the change is still a BLOCK"
            $ findings Set.empty (scanned (TableName "ledger.renamed_file") oldOid)
            @?= [("lock/observed-access-exclusive", SeverityBlock)]
        ]
  where
    oldTable = TableName "ledger.bank_statement_file"
    newTable = TableName "ledger.card_statement_file"
    oldOid = RelOid 1001
    newOid = RelOid 2002
    existing = Set.singleton oldOid
    stat oid scans = TableStat{relOid = oid, relFileNode = RelFileNode 1, seqScans = scans}
    scanned tname oid = (Map.singleton tname (stat oid 0), Map.singleton tname (stat oid 1))
    findings locks (statsBefore, statsAfter) =
        map summary (lockFindings range locks (computeImpacts existing statsBefore statsAfter))
    range = SourceRange{path = "test.sql", startLine = 1, startCol = 1, endLine = 1, endCol = 1}
    summary Finding{rule, severity} = (renderRule rule, severity)
