module Ribosome.Test.Unit where

import Control.Exception.Lifted (bracket_)
import Hedgehog (TestT)
import Hedgehog.Internal.Property (mkTestT, runTestT)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.Log ()
import System.Log.Logger (Priority(DEBUG, WARNING), setLevel, updateGlobalLogger)

import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE, pluginName)
import Ribosome.Control.Ribosome (Ribosome)
import Ribosome.Error.Report.Class (ReportError)
import Ribosome.Plugin.RpcHandler (RpcHandler)
import Ribosome.Test.Embed (Runner, TestConfig(..), setupPluginEnv, unsafeEmbeddedSpecR)
import qualified Ribosome.Test.File as F (fixture, fixtureContent, tempDir)
import Ribosome.Test.Orphans ()

uPrefix :: Text
uPrefix = "test"

uTest :: (MonadIO m, NvimE e m) => Runner m
uTest conf spec = do
  setupPluginEnv conf
  spec

unitTest ::
  MonadIO n =>
  MonadIO m =>
  NvimE e' n =>
  MonadFail m =>
  ReportError e =>
  MonadBaseControl IO m =>
  RpcHandler e (Ribosome env) n =>
  TestConfig ->
  env ->
  TestT n a ->
  TestT m a
unitTest cfg env t = do
  mkTestT (unsafeEmbeddedSpecR uTest cfg env (runTestT t))

unitSpecDef ::
  MonadIO n =>
  MonadIO m =>
  NvimE e' n =>
  MonadFail m =>
  ReportError e =>
  MonadBaseControl IO m =>
  RpcHandler e (Ribosome env) n =>
  env ->
  TestT n a ->
  TestT m a
unitSpecDef =
  unitTest def

unitSpecDef' ::
  MonadIO n =>
  MonadIO m =>
  NvimE e' n =>
  MonadFail m =>
  ReportError e =>
  MonadBaseControl IO m =>
  RpcHandler e (Ribosome ()) n =>
  TestT n a ->
  TestT m a
unitSpecDef' =
  unitSpecDef ()

tempDir :: MonadIO m => FilePath -> m FilePath
tempDir = F.tempDir uPrefix

tempFile :: MonadIO m => FilePath -> m FilePath
tempFile file = do
  absDir <- tempDir $ takeDirectory file
  return $ absDir </> takeFileName file

fixture :: MonadIO m => FilePath -> m FilePath
fixture = F.fixture uPrefix

fixtureContent :: MonadIO m => FilePath -> m Text
fixtureContent = F.fixtureContent uPrefix

withLogAs ::
  MonadIO m =>
  MonadBaseControl IO m =>
  Text ->
  m a ->
  m a
withLogAs name =
  bracket_ (logLevel DEBUG) (logLevel WARNING)
  where
    logLevel =
      liftIO . updateGlobalLogger (toString name) . setLevel

withLog ::
  MonadRibo m =>
  MonadBaseControl IO m =>
  m a ->
  m a
withLog thunk =
  (`withLogAs` thunk) =<< pluginName
