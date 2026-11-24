{-# LANGUAGE NoFieldSelectors #-}

-- | Parse and apply @-- lint:ignore reason="…"@ annotations.
--
-- Annotation applies to the next non-comment, non-blank line. The
-- @reason="…"@ field is mandatory and surfaces in the audit log.
module Sqint.Suppression (
    Annotation (..),
    parseAnnotations,
    applyAnnotations,
) where

import Data.Text qualified as T
import RIO
import Sqint.Diagnostic (Finding (..), Severity (..), SourceRange (..))
import Sqint.Rule (Rule (..))


data Annotation = Annotation
    { line :: Int
    , reason :: Text
    }


-- | Scan a SQL source for @-- lint:ignore reason="…"@ comments.
--
-- Each annotation is recorded at the *target* line — the next line
-- after the comment that has non-comment, non-whitespace content.
-- If the annotation sits at the end of a SQL line it applies to that
-- line directly.
--
-- Annotations missing a @reason="…"@ field are dropped and surfaced
-- as WARN-severity findings so the author knows the annotation did
-- not suppress anything.
parseAnnotations :: FilePath -> Text -> ([Annotation], [Finding])
parseAnnotations path src =
    let numbered = zip [1 :: Int ..] (T.lines src)
        extracted = mapMaybe extract numbered
        (valid, malformed) = partitionEithers (map validate extracted)
     in (resolveTargets numbered valid, map (malformedFinding path) malformed)


extract :: (Int, Text) -> Maybe (Int, Bool, Maybe Text)
extract (lineNum, lineText) = do
    let (before, comment) = T.breakOn "-- lint:ignore" lineText
    guard (not (T.null comment))
    let rest = T.drop (T.length "-- lint:ignore") comment
    let reason = extractReason rest
    let trailing = T.strip before
    pure (lineNum, T.null trailing, reason)


validate :: (Int, Bool, Maybe Text) -> Either (Int, Bool, Text) Int
validate (lineNum, isStandalone, mReason) = case mReason of
    Just reason | not (T.null reason) -> Left (lineNum, isStandalone, reason)
    _ -> Right lineNum


extractReason :: Text -> Maybe Text
extractReason rest =
    case T.stripPrefix "reason=\"" (T.dropWhile (== ' ') rest) of
        Just inner ->
            let (quoted, _) = T.breakOn "\"" inner
             in Just quoted
        Nothing -> Nothing


malformedFinding :: FilePath -> Int -> Finding
malformedFinding path lineNum =
    Finding
        { range = SourceRange{path, startLine = lineNum, startCol = 1, endLine = lineNum, endCol = 1}
        , severity = SeverityWarn
        , rule = SuppressionMissingReason
        , title = "lint:ignore annotation missing reason=\"…\""
        , detail = "Annotation is ignored. Add `reason=\"why this is safe\"` so the suppression is audited."
        , suppressedBy = Nothing
        }


-- | If the annotation is on its own line, advance to the next non-blank,
-- non-comment line. If the annotation is trailing on a SQL line, it
-- applies to that line.
resolveTargets :: [(Int, Text)] -> [(Int, Bool, Text)] -> [Annotation]
resolveTargets allLines = map resolve
  where
    resolve (lineNum, isStandalone, reason)
        | isStandalone =
            case dropWhile isBlankOrComment (dropWhile ((<= lineNum) . fst) allLines) of
                ((target, _) : _) -> Annotation{line = target, reason}
                [] -> Annotation{line = lineNum, reason}
        | otherwise = Annotation{line = lineNum, reason}

    isBlankOrComment (_, txt) =
        let s = T.strip txt
         in T.null s || "--" `T.isPrefixOf` s


-- | Mark findings as suppressed when an annotation lands on any line of
-- their range: the line below a standalone annotation is a statement's
-- first, and an annotation trailing a statement sits on its last.
applyAnnotations :: [Annotation] -> [Finding] -> [Finding]
applyAnnotations annotations = map mark
  where
    mark f =
        case [a | a <- annotations, f.range.startLine <= a.line, a.line <= f.range.endLine] of
            (a : _) -> f{suppressedBy = Just a.reason}
            [] -> f
