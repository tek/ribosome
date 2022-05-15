module Ribosome.File where

import qualified Data.Text as Text
import System.Directory (canonicalizePath, getHomeDirectory)

canonicalPathWithHome :: FilePath -> FilePath -> FilePath
canonicalPathWithHome home path =
  toString (Text.replace "~" (toText home) (toText path))

canonicalPath :: MonadIO m => FilePath -> m FilePath
canonicalPath path = do
  home <- liftIO getHomeDirectory
  return $ canonicalPathWithHome home path

canonicalPaths :: MonadIO m => [FilePath] -> m [FilePath]
canonicalPaths paths = do
  home <- liftIO getHomeDirectory
  let withHome = fmap (canonicalPathWithHome home) paths
  liftIO $ mapM canonicalizePath withHome
