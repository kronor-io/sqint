{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | The deploy tree, read once per run.
--
-- Two static rules need a fact about the whole schema rather than about
-- the files a run was asked to lint: which domains carry a CHECK (see
-- "Sqint.Domains") and which tables have a registered deferred
-- backfill (see "Sqint.DeferredDependency"). Each used to walk
-- and read the deploy tree for itself, so every run read every
-- file twice and case-folded them twice — together more than everything
-- else a run does.
module Sqint.DeployTree (
    DeployFile (..),
    loadDeployTree,
) where

import Data.List (sort)
import Data.Text qualified as T
import RIO
import RIO.Directory (doesDirectoryExist, listDirectory)
import RIO.FilePath ((</>))


data DeployFile = DeployFile
    { path :: !FilePath
    , source :: !Text
    , lowered :: !Text
    -- ^ 'source' case-folded once. Both scanners match lowercase SQL
    -- keywords against it before doing anything expensive with
    -- 'source', and folding five megabytes of migrations per scanner
    -- is not free.
    }


-- | Every @.sql@ file under @\<databaseRoot\>\/deploy@, sorted, so that
-- whatever is built from it is stable.
--
-- Siblings of @deploy@ (@revert@, @verify@, @test@) are deliberately not
-- read: they hold rewrites and intentionally-invalid SQL that would
-- confuse both scanners.
--
-- A deploy script that is not valid UTF-8 fails the run, loudly and by
-- path. Nothing here would know what to do with another encoding, and
-- skipping such a file quietly
-- would make the rules built on this tree report a clean bill of health
-- for a file they never read.
loadDeployTree :: FilePath -> IO [DeployFile]
loadDeployTree databaseRoot = do
    paths <- sqlFilesUnder (databaseRoot </> "deploy")
    traverse readDeployFile paths


readDeployFile :: FilePath -> IO DeployFile
readDeployFile path = do
    source <- readFileUtf8 path
    pure DeployFile{path, source, lowered = T.toLower source}


sqlFilesUnder :: FilePath -> IO [FilePath]
sqlFilesUnder top = do
    exists <- doesDirectoryExist top
    if exists then sqlFilesIn top else pure []


sqlFilesIn :: FilePath -> IO [FilePath]
sqlFilesIn dir = do
    entries <- listDirectory dir
    concat <$> traverse (classify dir) (sort entries)


classify :: FilePath -> FilePath -> IO [FilePath]
classify dir entry = do
    let full = dir </> entry
    isDir <- doesDirectoryExist full
    if isDir
        then sqlFilesIn full
        else pure [full | ".sql" `T.isSuffixOf` T.pack entry]
