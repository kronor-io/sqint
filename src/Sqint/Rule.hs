{-# LANGUAGE OverloadedStrings #-}

-- | Every rule the linter can report, and the @family/rule@ ID it is
-- printed as.
--
-- The family says which part of the linter found it:
--
--   * @lock/@ — a lock a statement takes. Predicted from the SQL by the
--     static pass, or observed by deploying it (@lock/observed-…@).
--   * @deploy/@ — the dynamic pass deploying the branch's changes.
--   * @function/@ — SQL functions a change leaves broken.
--   * @index/@ — predicates that may stop a query using an index.
--   * @txn/@, @deferred/@, @suppression/@ — one rule each.
--   * @setup/@ — the linter could not run a check; nothing is known to
--     be wrong with the migration.
--
-- IDs are what people read in CI output and the JSON report, so a new
-- rule picks a family here rather than a new prefix.
module Sqint.Rule (
    Rule (..),
    renderRule,
    isPredictedLock,
    isObservedLock,
) where

import RIO
import Sqint.Lock (LockClass (..))


data Rule
    = LockAddColumn
    | LockAlterColumnType
    | LockAddConstraint
    | LockAlterDomain
    | LockCreateIndex
    | LockDropIndex
    | LockObserved LockClass
    | DeployNonTransactional
    | DeployStatementNotObserved
    | DeployFailed
    | FunctionBrokenBody
    | FunctionCheckSkipped
    | IndexCastOnColumn
    | IndexFunctionOnColumn
    | IndexDefeated
    | IndexPlanCheckSkipped
    | TxnSetLocalAcrossCommit
    | DeferredUnfinishedBackfill
    | SuppressionMissingReason
    | SetupUnparsableSql
    | SetupUnparsableDomainFile
    | SetupUnparsablePlan
    | SetupBaselineMissing
    deriving stock (Eq, Show)


renderRule :: Rule -> Text
renderRule = \case
    LockAddColumn -> "lock/add-column"
    LockAlterColumnType -> "lock/alter-column-type"
    LockAddConstraint -> "lock/add-constraint"
    LockAlterDomain -> "lock/alter-domain"
    LockCreateIndex -> "lock/create-index"
    LockDropIndex -> "lock/drop-index"
    LockObserved cls -> "lock/observed-" <> lockMode cls
    DeployNonTransactional -> "deploy/non-transactional"
    DeployStatementNotObserved -> "deploy/statement-not-observed"
    DeployFailed -> "deploy/failed"
    FunctionBrokenBody -> "function/broken-body"
    FunctionCheckSkipped -> "function/check-skipped"
    IndexCastOnColumn -> "index/cast-on-column"
    IndexFunctionOnColumn -> "index/function-on-column"
    IndexDefeated -> "index/defeated"
    IndexPlanCheckSkipped -> "index/plan-check-skipped"
    TxnSetLocalAcrossCommit -> "txn/set-local-across-commit"
    DeferredUnfinishedBackfill -> "deferred/unfinished-backfill"
    SuppressionMissingReason -> "suppression/missing-reason"
    SetupUnparsableSql -> "setup/unparsable-sql"
    SetupUnparsableDomainFile -> "setup/unparsable-domain-file"
    SetupUnparsablePlan -> "setup/unparsable-plan"
    SetupBaselineMissing -> "setup/baseline-missing"


-- | PostgreSQL's lock mode names (@ACCESS EXCLUSIVE@ and so on), in the
-- ID's lower-case, hyphenated style.
lockMode :: LockClass -> Text
lockMode = \case
    AccessShare -> "access-share"
    RowShare -> "row-share"
    RowExclusive -> "row-exclusive"
    ShareUpdateExclusive -> "share-update-exclusive"
    Share -> "share"
    ShareRowExclusive -> "share-row-exclusive"
    Exclusive -> "exclusive"
    AccessExclusive -> "access-exclusive"


-- | A lock the static pass predicted from the SQL alone.
isPredictedLock :: Rule -> Bool
isPredictedLock = \case
    LockAddColumn -> True
    LockAlterColumnType -> True
    LockAddConstraint -> True
    LockAlterDomain -> True
    LockCreateIndex -> True
    LockDropIndex -> True
    _ -> False


-- | A lock the dynamic pass saw a statement take.
isObservedLock :: Rule -> Bool
isObservedLock = \case
    LockObserved _ -> True
    _ -> False
