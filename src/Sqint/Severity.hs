module Sqint.Severity (
    Severity (..),
    severityRank,
) where

import RIO


data Severity
    = SeverityBlock
    | SeverityWarn
    | SeverityInfo
    deriving stock (Eq, Show)


-- | Higher rank = more severe. Used to keep the strongest finding when
-- the static and dynamic passes both flag one source location.
severityRank :: Severity -> Int
severityRank = \case
    SeverityBlock -> 2
    SeverityWarn -> 1
    SeverityInfo -> 0
