module Ribosome.Test.File(
  tempDirIO,
  tempDir,
  fixture,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Directory (canonicalizePath, createDirectoryIfMissing, removePathForcibly)
import System.FilePath ((</>))

testDir :: Text -> IO FilePath
testDir prefix = canonicalizePath $ "test" </> (toString prefix)

-- raises exception if cwd is not the package root so we don't damage anything
tempDirIO :: Text -> FilePath -> IO FilePath
tempDirIO prefix path = do
  base <- testDir prefix
  let dir = base </> "temp"
  removePathForcibly dir
  createDirectoryIfMissing False dir
  let absPath = dir </> path
  createDirectoryIfMissing True absPath
  return absPath

tempDir :: MonadIO m => Text -> FilePath -> m FilePath
tempDir prefix path =
  liftIO $ tempDirIO prefix path

fixture :: MonadIO m => Text -> FilePath -> m FilePath
fixture prefix path = do
  base <- liftIO $ testDir prefix
  return $ base </> "fixtures" </> path
