module Ribosome.File(
  canonicalPathWithHome,
  canonicalPath,
  canonicalPaths,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.String.Utils (replace)
import System.Directory (getHomeDirectory, canonicalizePath)

canonicalPathWithHome :: FilePath -> FilePath -> FilePath
canonicalPathWithHome = replace "~"

canonicalPath :: MonadIO m => FilePath -> m FilePath
canonicalPath path = do
  home <- liftIO getHomeDirectory
  return $ canonicalPathWithHome home path

canonicalPaths :: MonadIO m => [FilePath] -> m [FilePath]
canonicalPaths paths = do
  home <- liftIO getHomeDirectory
  let withHome = fmap (canonicalPathWithHome home) paths
  liftIO $ mapM canonicalizePath withHome
