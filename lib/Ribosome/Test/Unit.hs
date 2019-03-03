module Ribosome.Test.Unit(
  unitSpec,
  tempDir,
  tempFile,
  uPrefix,
  fixture,
  unitSpecR,
  unitSpecE,
) where

import Control.Monad.IO.Class (MonadIO)
import Neovim (Neovim)
import System.FilePath (takeDirectory, takeFileName, (</>))
import UnliftIO.Exception (throwString)

import Ribosome.Control.Monad.Ribo (ConcNvimS, Ribo, RiboE, riboE2ribo)
import Ribosome.Test.Embed (TestConfig(..), setupPluginEnv, unsafeEmbeddedSpec, unsafeEmbeddedSpecR)
import qualified Ribosome.Test.File as F (fixture, tempDir)

uPrefix :: String
uPrefix = "u"

uSpec :: TestConfig -> Neovim env () -> Neovim env ()
uSpec conf spec = do
  setupPluginEnv conf
  spec

unitSpec :: TestConfig -> e -> Neovim e () -> IO ()
unitSpec =
  unsafeEmbeddedSpec uSpec

unitSpecR :: TestConfig -> s -> Ribo s (ConcNvimS s) () -> IO ()
unitSpecR =
  unsafeEmbeddedSpecR uSpec

unitSpecE :: Show e => TestConfig -> s -> RiboE s e (ConcNvimS s) () -> IO ()
unitSpecE conf s spec =
  unitSpecR conf s wrapped
  where
    wrapped = do
      result <- riboE2ribo spec
      case result of
        Right _ -> return ()
        Left e -> throwString $ "unitSpecE failed with Left: " ++ show e

tempDir :: FilePath -> Neovim e FilePath
tempDir = F.tempDir uPrefix

tempFile :: FilePath -> Neovim e FilePath
tempFile file = do
  absDir <- tempDir $ takeDirectory file
  return $ absDir </> takeFileName file

fixture :: MonadIO m => FilePath -> m FilePath
fixture = F.fixture uPrefix
