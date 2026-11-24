{-# LANGUAGE NoFieldSelectors #-}

module Sqint.Diagnostic (
    Severity (..),
    Finding (..),
    SourceRange (..),
    severityExitCode,
    severityRank,
    isBlocking,
) where

import RIO
import Sqint.Rule (Rule)
import Sqint.Severity (Severity (..), severityRank)


data SourceRange = SourceRange
    { path :: FilePath
    , startLine :: Int
    , startCol :: Int
    , endLine :: Int
    , endCol :: Int
    }
    deriving stock (Eq, Show)


data Finding = Finding
    { range :: SourceRange
    , severity :: Severity
    , rule :: Rule
    , title :: Text
    , detail :: Text
    , suppressedBy :: Maybe Text
    }
    deriving stock (Eq, Show)


isBlocking :: Finding -> Bool
isBlocking f =
    f.severity == SeverityBlock && isNothing f.suppressedBy


severityExitCode :: [Finding] -> ExitCode
severityExitCode findings
    | any isBlocking findings = ExitFailure 1
    | otherwise = ExitSuccess
