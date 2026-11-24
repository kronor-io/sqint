{-# LANGUAGE NoFieldSelectors #-}

-- | Lock-class taxonomy and severity policy.
--
-- Mirrors PostgreSQL's documented lock modes (PG docs ch. 13.3 and the
-- ALTER TABLE notes). Each statement we lint is mapped to a
-- @(LockClass, RewriteImpact)@ pair; the severity policy below turns
-- that pair into a 'Diagnostic.Severity'.
module Sqint.Lock (
    LockClass (..),
    RewriteImpact (..),
    LockProfile (..),
    severityFor,
    parseLockMode,
    lockClassName,
) where

import RIO
import Sqint.Severity (Severity (..))


-- | PostgreSQL lock modes, listed in the declaration from least to most
-- blocking.
data LockClass
    = AccessShare
    | RowShare
    | RowExclusive
    | ShareUpdateExclusive
    | Share
    | ShareRowExclusive
    | Exclusive
    | AccessExclusive
    deriving stock (Eq, Show)


-- | What the statement does to row storage.
data RewriteImpact
    = NoImpact
    | TableScan
    | TableRewrite
    deriving stock (Eq)


data LockProfile = LockProfile
    { lockClass :: LockClass
    , rewriteImpact :: RewriteImpact
    }


-- | Single severity policy applied to every classified statement.
--
-- What makes a lock dangerous is not its /mode/ but whether it is held
-- while the statement does real work. A lock taken for a catalog-only
-- change (ADD COLUMN without default, ADD CONSTRAINT ... NOT VALID, a
-- NOT-VALID foreign key, DROP COLUMN/INDEX) is released in
-- microseconds and is safe whatever its mode — so 'NoImpact' is always
-- informational. A scan or a rewrite is what holds the lock long
-- enough to block other sessions, and then the mode decides how badly:
-- under AccessExclusiveLock everything blocks (BLOCK); under a
-- write-blocking mode (Share/ShareRowExclusive/Exclusive, e.g. a
-- non-concurrent CREATE INDEX) writers block (WARN); under
-- ShareUpdateExclusive or weaker (e.g. CREATE INDEX CONCURRENTLY,
-- VALIDATE CONSTRAINT) normal traffic proceeds (INFO).
severityFor :: LockProfile -> Severity
severityFor LockProfile{lockClass, rewriteImpact} = case rewriteImpact of
    NoImpact -> SeverityInfo
    TableRewrite -> SeverityBlock
    TableScan -> scanSeverity lockClass
  where
    scanSeverity = \case
        AccessExclusive -> SeverityBlock
        Exclusive -> SeverityWarn
        ShareRowExclusive -> SeverityWarn
        Share -> SeverityWarn
        ShareUpdateExclusive -> SeverityInfo
        RowExclusive -> SeverityInfo
        RowShare -> SeverityInfo
        AccessShare -> SeverityInfo


-- | Parse a lock-mode name as it appears in @pg_locks.mode@ (the trailing
-- @Lock@ suffix is what the catalog uses, e.g. @AccessExclusiveLock@).
parseLockMode :: Text -> Maybe LockClass
parseLockMode = \case
    "AccessShareLock" -> Just AccessShare
    "RowShareLock" -> Just RowShare
    "RowExclusiveLock" -> Just RowExclusive
    "ShareUpdateExclusiveLock" -> Just ShareUpdateExclusive
    "ShareLock" -> Just Share
    "ShareRowExclusiveLock" -> Just ShareRowExclusive
    "ExclusiveLock" -> Just Exclusive
    "AccessExclusiveLock" -> Just AccessExclusive
    _ -> Nothing


-- | The catalog spelling of a 'LockClass' (no @Lock@ suffix). Used in
-- diagnostic messages.
lockClassName :: LockClass -> Text
lockClassName = \case
    AccessShare -> "AccessShare"
    RowShare -> "RowShare"
    RowExclusive -> "RowExclusive"
    ShareUpdateExclusive -> "ShareUpdateExclusive"
    Share -> "Share"
    ShareRowExclusive -> "ShareRowExclusive"
    Exclusive -> "Exclusive"
    AccessExclusive -> "AccessExclusive"
