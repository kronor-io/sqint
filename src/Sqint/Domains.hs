{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Discover which PostgreSQL domains have a CHECK constraint.
--
-- The lock-class linter needs this to surface the
-- @ADD COLUMN \<domain-with-CHECK\>@ case: adding a column whose
-- declared type is a CHECK-bearing domain forces PostgreSQL to
-- validate every existing row under @AccessExclusiveLock@. Without
-- the domain set the linter would either emit false positives on
-- harmless typed columns or false negatives on the dangerous ones.
--
-- The loader looks at every @.sql@ file in the deploy tree — not only
-- the files under @deploy/type/@ — because a migration can create a
-- domain inline.
-- Parse failures are surfaced as a @WARN@ finding instead of
-- silently skipping the file, so reviewers know the rule may produce
-- false negatives.
module Sqint.Domains (
    DomainSet (..),
    loadDomainSet,
) where

import Data.Set qualified as Set
import Data.Text qualified as T
import PgQuery qualified as Pg
import RIO
import Sqint.DeployTree (DeployFile (..))
import Sqint.Diagnostic (Finding (..), Severity (..), SourceRange (..))
import Sqint.Rule (Rule (..))


data DomainSet = DomainSet
    { withCheck :: Set Text
    -- ^ Domain names that have at least one CHECK constraint
    -- attached. Names are lower-cased; the linter compares against
    -- the parsed column type name (also lower-cased) when applying
    -- the rule.
    , parseFailures :: [Finding]
    -- ^ Files we could not parse; tracking them lets the caller
    -- surface a single per-file warning instead of one per
    -- statement.
    }


-- | Pick the CHECK-bearing domains out of a deploy tree read by
-- "Sqint.DeployTree", which is what confines this to
-- @deploy@ — siblings (@revert/@, @verify/@, @test/@) may hold
-- rewrites or intentionally-invalid SQL and are never read.
loadDomainSet :: [DeployFile] -> IO DomainSet
loadDomainSet deployTree = do
    results <- traverse scanOne deployTree
    pure
        DomainSet
            { withCheck = Set.unions (map (.withCheck) results)
            , parseFailures = concatMap (.parseFailures) results
            }


scanOne :: DeployFile -> IO DomainSet
scanOne file = do
    -- Skip files that can't define a domain. Sqitch deploy scripts
    -- contain psql client metacommands (`\timing`, `\gset`) and
    -- PG18-only syntax (`GENERATED … VIRTUAL`, top-level COMMIT) that
    -- libpg_query can't read; parsing those just to discover they
    -- have no @CREATE DOMAIN@ wastes work and floods CI with
    -- WARN findings the linter has no real complaint about.
    if not ("create domain" `T.isInfixOf` file.lowered)
        then pure DomainSet{withCheck = mempty, parseFailures = []}
        else do
            result <- Pg.parseSql (T.unpack file.source)
            case result of
                Left err ->
                    pure
                        DomainSet
                            { withCheck = mempty
                            , parseFailures = [parseFailure file.path err]
                            }
                Right parsed ->
                    pure
                        DomainSet
                            { withCheck = Set.fromList (mapMaybe domainFromStmt (parsed ^.. Pg.stmts . traverse . Pg.stmt))
                            , parseFailures = []
                            }


parseFailure :: FilePath -> String -> Finding
parseFailure path err =
    Finding
        { range = SourceRange{path, startLine = 1, startCol = 1, endLine = 1, endCol = 1}
        , -- Informational, not a warning: this fires for pre-existing
          -- domain-definition files the linter can't fully parse, on
          -- every run regardless of what the PR changed, so it is
          -- constant noise rather than something to act on in a given
          -- PR. It still surfaces under --verbose.
          severity = SeverityInfo
        , rule = SetupUnparsableDomainFile
        , title = "could not parse domain source"
        , detail =
            "libpg_query: "
                <> T.pack err
                <> " — the lock-class rule for ADD COLUMN with a domain type may produce "
                <> "false negatives for domains declared in this file."
        , suppressedBy = Nothing
        }


-- | Pull the (lower-cased) domain name out of a CreateDomainStmt that
-- carries at least one CHECK constraint. Other CONSTR types
-- (NOT NULL, DEFAULT) are not relevant for the rewrite/scan rule.
domainFromStmt :: Pg.Node -> Maybe Text
domainFromStmt node = do
    stmt <- view Pg.maybe'createDomainStmt node
    let constraints =
            mapMaybe
                (view Pg.maybe'constraint)
                (stmt ^.. Pg.constraints . traverse)
    guard (any (\c -> c ^. Pg.contype == Pg.CONSTR_CHECK) constraints)
    case stmt ^.. Pg.domainname . traverse . Pg.string . Pg.sval of
        [] -> Nothing
        parts -> Just (T.toLower (T.intercalate "." parts))
