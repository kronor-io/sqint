{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Data.Version (makeVersion)
import OptEnvConf
import RIO hiding (Reader)
import RIO.FilePath ((</>))
import Sqint.Deploy.Connection (DeploySettings, parseSettings)
import Sqint.Output (OutputFormat (..))
import Sqint.Run (LintConfig (..), runLint)
import System.Exit qualified as Exit


data Args = Args
    { outputFormat :: OutputFormat
    , databaseRoot :: FilePath
    , planFile :: Maybe FilePath
    , baselineChange :: Maybe Text
    , verbose :: Bool
    , withPostgres :: Maybe DeploySettings
    , deferredBackfills :: Bool
    , paths :: [FilePath]
    }


instance HasParser Args where
    settingsParser = do
        outputFormat <-
            setting
                [ help "Output format: text or json"
                , long "format"
                , reader outputFormatReader
                , metavar "FORMAT"
                , option
                , value OutputText
                ]
        databaseRoot <-
            setting
                [ help "The sqitch top directory: holds deploy/ and, by default, sqitch.plan"
                , long "database-root"
                , reader str
                , metavar "DIR"
                , option
                , value "database"
                ]
        planFile <-
            optional
                $ setting
                    [ help "Path to sqitch.plan, which orders the new changes for the dynamic pass (default: DIR/sqitch.plan)"
                    , long "plan-file"
                    , reader str
                    , metavar "FILE"
                    , option
                    ]
        baselineChange <-
            optional
                $ setting
                    [ help
                        "Target branch's plan head (sqitch change name). Everything\
                        \ after it in the plan is treated as new in this PR and\
                        \ observed cumulatively. Required for the dynamic pass."
                    , long "baseline-change"
                    , reader (fromString <$> str)
                    , metavar "CHANGE"
                    , option
                    ]
        verbose <-
            setting
                [ help "Show informational and suppressed findings too (default: only BLOCK/WARN)"
                , long "verbose"
                , switch True
                , value False
                ]
        withPostgres <-
            optional
                $ setting
                    [ help
                        "libpq connection string (DSN/URI) for the dynamic\
                        \ pass. When set, the linter runs both the static\
                        \ AST classifier and the dynamic pass; when unset,\
                        \ static-only."
                    , long "with-postgres"
                    , reader deploySettingsReader
                    , metavar "DSN"
                    , option
                    ]
        deferredBackfills <-
            setting
                [ help
                    "Check that a change finishes the deferred backfills registered\
                    \ on a table (deferred.migration) before validating a constraint\
                    \ or setting NOT NULL there"
                , long "deferred-backfills"
                , switch True
                , value False
                ]
        paths <-
            many
                $ setting
                    [ help "SQL files to lint"
                    , reader str
                    , argument
                    , metavar "FILE"
                    ]
        pure Args{outputFormat, databaseRoot, planFile, baselineChange, verbose, withPostgres, deferredBackfills, paths}


outputFormatReader :: Reader OutputFormat
outputFormatReader = maybeReader \case
    "text" -> Just OutputText
    "json" -> Just OutputJson
    _ -> Nothing


deploySettingsReader :: Reader DeploySettings
deploySettingsReader = eitherReader parseSettings


main :: IO ()
main = do
    args :: Args <- runSettingsParser (makeVersion [0, 1]) "Static + dynamic linter for sqitch migrations on PostgreSQL"
    Exit.exitWith
        =<< runLint
            LintConfig
                { paths = args.paths
                , outputFormat = args.outputFormat
                , verbose = args.verbose
                , databaseRoot = args.databaseRoot
                , planPath = fromMaybe (args.databaseRoot </> "sqitch.plan") args.planFile
                , baselineChange = args.baselineChange
                , postgres = args.withPostgres
                , deferredBackfills = args.deferredBackfills
                }
