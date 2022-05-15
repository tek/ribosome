module Ribosome.Test.Unit where

import Control.Exception.Lifted (bracket_)
import Hedgehog (TestT)
import Hedgehog.Internal.Property (mkTestT, runTestT)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.Log ()
import System.Log.Logger (Priority(DEBUG, WARNING), setLevel, updateGlobalLogger)

import Ribosome.Control.Ribosome (Ribosome)
import Ribosome.Error.Report.Class (ReportError)
import Ribosome.Plugin.RpcHandler (RpcHandler)
import Ribosome.Test.Embed (Runner, TestConfig(..), setupPluginEnv, unsafeEmbeddedTestR)
import qualified Ribosome.Test.File as F (fixture, fixtureContent, tempDir)
import Ribosome.Test.Orphans ()

uPrefix :: Text
uPrefix = "test"

uTest :: (MonadIO m, Member Rpc r) => Runner m
uTest conf spec = do
  setupPluginEnv conf
  spec

unitTest ::
  MonadIO n =>
  MonadIO m =>
  NvimE e' n =>
  MonadFail m =>
  ReportError e =>
  RpcHandler e (Ribosome env) n =>
  TestConfig ->
  env ->
  TestT n a ->
  TestT m a
unitTest cfg env t = do
  mkTestT (unsafeEmbeddedTestR uTest cfg env (runTestT t))

unitTestDef ::
  MonadIO n =>
  MonadIO m =>
  NvimE e' n =>
  MonadFail m =>
  ReportError e =>
  RpcHandler e (Ribosome env) n =>
  env ->
  TestT n a ->
  TestT m a
unitTestDef =
  unitTest def

unitTestDef' ::
  MonadIO n =>
  MonadIO m =>
  NvimE e' n =>
  MonadFail m =>
  ReportError e =>
  RpcHandler e (Ribosome ()) n =>
  TestT n a ->
  TestT m a
unitTestDef' =
  unitTestDef ()

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
  Text ->
  m a ->
  m a
withLogAs name =
  bracket_ (logLevel DEBUG) (logLevel WARNING)
  where
    logLevel =
      liftIO . updateGlobalLogger (toString name) . setLevel

withLog ::
  m a ->
  m a
withLog thunk =
  (`withLogAs` thunk) =<< pluginName
