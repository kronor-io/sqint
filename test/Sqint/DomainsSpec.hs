{-# LANGUAGE OverloadedStrings #-}

module Sqint.DomainsSpec (tests) where

import Data.ByteString qualified as BS
import Data.Set qualified as Set
import RIO
import RIO.Directory (createDirectoryIfMissing)
import RIO.FilePath ((</>))
import Sqint.DeployTree (loadDeployTree)
import Sqint.Diagnostic (Finding (..))
import Sqint.Domains (DomainSet (..), loadDomainSet)
import Sqint.Rule (renderRule)
import Test.Tasty
import Test.Tasty.HUnit


-- | A domain with no CHECK; the lock-class rule shouldn't care.
domainWithoutCheck :: Text
domainWithoutCheck =
    "create domain phone as text;"


-- | A domain with a CHECK, the kind the ADD COLUMN rule looks for.
domainWithCheck :: Text
domainWithCheck =
    "create domain ssn as text check (length(value) = 11);"


-- | A migration that creates a domain inline; the loader must look
-- outside @type/@ to find these.
inlineMigration :: Text
inlineMigration =
    "create domain canbeencrypted_20000 as text check (length(value) <= 20000);"


-- | A file that mentions `create domain` (so it isn't skipped by the
-- text-prefilter) but is malformed enough to fail libpg_query —
-- A parse failure must surface as a 'WARN' finding.
unparseable :: Text
unparseable =
    "create domain broken as ¯\\_(ツ)_/¯"


tests :: TestTree
tests =
    testGroup
        "Domains"
        [ testCase "non-existent root returns an empty set" $ do
            result <- loadDeployTree "/tmp/nonexistent-domains-fixture" >>= loadDomainSet
            result.withCheck @?= mempty
            result.parseFailures @?= []
        , testCase "loads check-bearing domains and skips plain ones"
            $ withSystemTempDirectory "lint-domains"
            $ \root -> do
                let typeDir = root </> "deploy" </> "type"
                createDirectoryIfMissing True typeDir
                writeFileUtf8 (typeDir </> "ssn.sql") domainWithCheck
                writeFileUtf8 (typeDir </> "phone.sql") domainWithoutCheck
                result <- loadDeployTree root >>= loadDomainSet
                result.withCheck @?= Set.fromList ["ssn"]
                result.parseFailures @?= []
        , testCase "scans every .sql under the root, not only type/"
            $ withSystemTempDirectory "lint-domains"
            $ \root -> do
                let typeDir = root </> "deploy" </> "type"
                    migDir = root </> "deploy" </> "migrations" </> "2024" </> "11"
                createDirectoryIfMissing True typeDir
                createDirectoryIfMissing True migDir
                writeFileUtf8 (typeDir </> "ssn.sql") domainWithCheck
                writeFileUtf8 (migDir </> "inline.sql") inlineMigration
                result <- loadDeployTree root >>= loadDomainSet
                result.withCheck @?= Set.fromList ["canbeencrypted_20000", "ssn"]
        , testCase "siblings of deploy/ (revert, verify, test) are not scanned"
            $ withSystemTempDirectory "lint-domains"
            $ \root -> do
                let deployDir = root </> "deploy" </> "type"
                    revertDir = root </> "revert" </> "type"
                createDirectoryIfMissing True deployDir
                createDirectoryIfMissing True revertDir
                writeFileUtf8 (deployDir </> "ssn.sql") domainWithCheck
                -- revert/ would not redefine the domain in real life,
                -- but if it did, the loader must ignore it.
                writeFileUtf8 (revertDir </> "ghost.sql") "create domain ghost as text check (length(value) > 0);"
                result <- loadDeployTree root >>= loadDomainSet
                result.withCheck @?= Set.fromList ["ssn"]
        , testCase "a deploy script that is not UTF-8 fails the run"
            $ withSystemTempDirectory "lint-domains"
            $ \root -> do
                let typeDir = root </> "deploy" </> "type"
                createDirectoryIfMissing True typeDir
                writeFileUtf8 (typeDir </> "ssn.sql") domainWithCheck
                BS.writeFile (typeDir </> "undecodable.sql") (BS.pack [0x63, 0x72, 0xff, 0xfe, 0x65])
                -- Skipping it quietly would let the ADD COLUMN rule report a
                -- clean bill of health for a file it never read. Every text
                -- file in the repository is UTF-8, so this is a broken tree,
                -- not a case to tolerate.
                loaded <- tryAny (loadDeployTree root >>= loadDomainSet)
                case loaded of
                    Left _ -> pure ()
                    Right _ -> assertFailure "expected the undecodable deploy script to fail the run"
        , testCase "parse failures surface a WARN finding"
            $ withSystemTempDirectory "lint-domains"
            $ \root -> do
                let typeDir = root </> "deploy" </> "type"
                createDirectoryIfMissing True typeDir
                writeFileUtf8 (typeDir </> "broken.sql") unparseable
                result <- loadDeployTree root >>= loadDomainSet
                length result.parseFailures @?= 1
                map (renderRule . (.rule)) result.parseFailures @?= ["setup/unparsable-domain-file"]
        ]
