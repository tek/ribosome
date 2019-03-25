module Ribosome.Test.Unit where

import Control.Monad.IO.Class (MonadIO)
import Neovim (Neovim)
import System.FilePath (takeDirectory, takeFileName, (</>))
import UnliftIO.Exception (throwString)

import Ribosome.Control.Monad.Ribo (ConcNvimS, NvimE, Ribo, RiboE, riboE2ribo)
import Ribosome.Control.Ribosome (Ribosome)
import Ribosome.Error.Report.Class (ReportError)
import Ribosome.Plugin (RpcHandler(native))
import Ribosome.Test.Embed (Runner, TestConfig(..), setupPluginEnv, unsafeEmbeddedSpecR)
import qualified Ribosome.Test.File as F (fixture, tempDir)

uPrefix :: String
uPrefix = "u"

uSpec :: (MonadIO m, NvimE e m) => Runner m
uSpec conf spec = do
  setupPluginEnv conf
  spec

unitSpec ::
  (RpcHandler e (Ribosome env) m, ReportError e, MonadIO m, NvimE e' m) =>
  TestConfig ->
  env ->
  m () ->
  IO ()
unitSpec =
  unsafeEmbeddedSpecR uSpec

tempDir :: FilePath -> Neovim e FilePath
tempDir = F.tempDir uPrefix

tempFile :: FilePath -> Neovim e FilePath
tempFile file = do
  absDir <- tempDir $ takeDirectory file
  return $ absDir </> takeFileName file

fixture :: MonadIO m => FilePath -> m FilePath
fixture = F.fixture uPrefix
