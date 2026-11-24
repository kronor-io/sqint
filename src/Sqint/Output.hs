{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}

module Sqint.Output (
    OutputFormat (..),
    renderFindings,
) where

import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import RIO
import Sqint.Diagnostic (Finding (..), Severity (..), SourceRange (..))
import Sqint.Rule (renderRule)


data OutputFormat = OutputText | OutputJson
    deriving stock (Show)


-- | Render findings for the chosen format. In text mode, unless
-- @verbose@ is set, only actionable findings (un-suppressed BLOCK/WARN)
-- are printed and the rest are collapsed into a one-line footer — the
-- informational lock observations and suppressed findings are real but
-- rarely what you act on, and printing them all buries the signal.
-- JSON always carries every finding so downstream tooling can filter.
renderFindings :: OutputFormat -> Bool -> [Finding] -> Text
renderFindings OutputJson _ fs =
    TE.decodeUtf8Lenient (BSL.toStrict (Aeson.encode (map findingJson fs)))
renderFindings OutputText verbose fs
    | verbose = T.unlines (map renderText fs)
    | otherwise =
        let visible = filter isActionable fs
            hidden = length fs - length visible
            footer = [hiddenFooter hidden | hidden > 0]
         in T.unlines (map renderText visible <> footer)


-- | A finding worth surfacing by default: a non-suppressed blocker or
-- warning.
isActionable :: Finding -> Bool
isActionable f = isNothing f.suppressedBy && f.severity `elem` [SeverityBlock, SeverityWarn]


hiddenFooter :: Int -> Text
hiddenFooter n =
    [i|#{n} informational or suppressed finding#{plural} hidden — pass --verbose to show.|]
  where
    plural :: Text
    plural = if n == 1 then "" else "s"


renderText :: Finding -> Text
renderText f =
    let prefix = maybe (renderSeverity f.severity) (const "SUPPRESSED") f.suppressedBy
        path = f.range.path
        line = f.range.startLine
        ruleId = renderRule f.rule
        title = f.title
        detailBody = T.intercalate "\n        " (T.lines (T.stripEnd f.detail))
        detail :: Text
        detail = if T.null f.detail then "" else [i|\n        #{detailBody}|]
        reason :: Text
        reason = maybe "" (\r -> [i|\n        reason: #{r}|]) f.suppressedBy
     in [i|#{prefix} #{path}:#{line} [#{ruleId}] #{title}#{detail}#{reason}|]


renderSeverity :: Severity -> Text
renderSeverity SeverityBlock = "BLOCK"
renderSeverity SeverityWarn = "WARN"
renderSeverity SeverityInfo = "INFO"


findingJson :: Finding -> Aeson.Value
findingJson f =
    object
        [ "path" .= f.range.path
        , "startLine" .= f.range.startLine
        , "startCol" .= f.range.startCol
        , "endLine" .= f.range.endLine
        , "endCol" .= f.range.endCol
        , "severity" .= renderSeverity f.severity
        , "ruleId" .= renderRule f.rule
        , "title" .= f.title
        , "detail" .= f.detail
        , "suppressed" .= isJust f.suppressedBy
        , "suppressedReason" .= f.suppressedBy
        ]
