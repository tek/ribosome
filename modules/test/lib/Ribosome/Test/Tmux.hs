module Ribosome.Test.Tmux where

import Chiasma.Command.Pane (sendKeys)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxId (PaneId(PaneId))
import Chiasma.Monad.Stream (runTmux)
import Chiasma.Native.Api (TmuxNative(TmuxNative))
import Chiasma.Test.Tmux (TmuxTestConf)
import qualified Chiasma.Test.Tmux as Chiasma (tmuxGuiSpec, tmuxSpec, tmuxSpec')
import Control.Monad.Trans.Except (runExceptT)
import Data.DeepPrisms (DeepPrisms)
import Data.Default (Default(def))
import Data.Functor (void)
import qualified Neovim.Context.Internal as Internal (
  Config,
  StateTransition(Quit),
  newConfig,
  retypeConfig,
  )
import Neovim.Plugin (Plugin)
import Neovim.Plugin.Internal (NeovimPlugin, wrapPlugin)
import Neovim.RPC.Common (RPCConfig, SocketType(UnixSocket), createHandle, newRPCConfig)
import System.FilePath ((</>))
import System.Process.Typed (
  Process,
  ProcessConfig,
  createPipe,
  getExitCode,
  getStdin,
  getStdout,
  proc,
  setStdin,
  setStdout,
  unsafeProcessHandle,
  withProcess,
  )
import UnliftIO (throwString)
import UnliftIO.Directory (doesPathExist)
import UnliftIO.Exception (bracket)
import UnliftIO.Temporary (withTempDirectory)

import Ribosome.Config.Setting (updateSetting)
import Ribosome.Config.Settings (tmuxSocket)
import Ribosome.Control.Concurrent.Wait (waitIOPredDef)
import Ribosome.Control.Monad.Ribo (Ribo)
import Ribosome.Control.Ribosome (Ribosome(Ribosome), newRibosomeTMVar)
import Ribosome.Error.Report.Class (ReportError)
import Ribosome.Nvim.Api.IO (vimSetVar)
import Ribosome.Nvim.Api.RpcCall (RpcError)
import Ribosome.Plugin.RpcHandler (RpcHandler)
import Ribosome.Test.Embed (
  Runner,
  TestConfig(..),
  runEmbeddedWithPlugin,
  runPlugin,
  runTest,
  startHandlers,
  testNvimProcessConfig,
  )
import Ribosome.Test.Orphans ()
import Ribosome.Test.Unit (tempDir, uSpec)

runSocketNvimHs ::
  (RpcHandler e env m, ReportError e) =>
  TestConfig ->
  env ->
  m () ->
  Handle ->
  IO ()
runSocketNvimHs conf ribo specThunk socket = do
  nvimConf <- Internal.newConfig (pure Nothing) newRPCConfig
  let testCfg = Internal.retypeConfig ribo nvimConf
  bracket (startHandlers socket socket conf nvimConf) id (const $ runTest conf testCfg specThunk)

externalNvimCmdline :: FilePath -> Text
externalNvimCmdline socket =
  "nvim --listen " <> toText socket <> " -n -u NONE -i NONE"

startNvimInTmux ::
  TmuxNative ->
  FilePath ->
  IO Handle
startNvimInTmux api temp = do
  void $ runExceptT @TmuxError $ runTmux api $ sendKeys (PaneId 0) [toString $ externalNvimCmdline socket]
  _ <- waitIOPredDef (pure socket) doesPathExist
  createHandle (UnixSocket socket)
  where
    socket = temp </> "nvim-socket"

runGui ::
  (RpcHandler e env m, ReportError e) =>
  TmuxNative ->
  FilePath ->
  TestConfig ->
  env ->
  m () ->
  IO ()
runGui api temp conf ribo specThunk =
  runSocketNvimHs conf ribo specThunk =<< startNvimInTmux api temp

unsafeGuiSpec ::
  (RpcHandler e env m, ReportError e) =>
  TmuxNative ->
  FilePath ->
  Runner m ->
  TestConfig ->
  env ->
  m () ->
  IO ()
unsafeGuiSpec api temp runner conf s specThunk =
  runGui api temp conf s $ runner conf specThunk

unsafeGuiSpecR ::
  (RpcHandler e (Ribosome env) m, ReportError e) =>
  TmuxNative ->
  FilePath ->
  Runner m ->
  TestConfig ->
  env ->
  m () ->
  IO ()
unsafeGuiSpecR api temp runner conf s specThunk = do
  tv <- newRibosomeTMVar s
  let ribo = Ribosome (tcPluginName conf) tv
  unsafeGuiSpec api temp runner conf ribo specThunk

guiSpec ::
  DeepPrisms e RpcError =>
  ReportError e =>
  TestConfig ->
  TmuxNative ->
  s ->
  Ribo s e () ->
  IO ()
guiSpec conf api env specThunk = do
  socketDir <- tempDir "gui-spec"
  withTempDirectory socketDir "spec" run
  where
    run tempdir =
      unsafeGuiSpecR api tempdir uSpec conf env specThunk

withTmux ::
  DeepPrisms e RpcError =>
  Ribo s e () ->
  TmuxNative ->
  Ribo s e ()
withTmux thunk (TmuxNative (Just socket)) =
  updateSetting tmuxSocket socket *> thunk
withTmux _ _ =
  throwString "no socket in test tmux"

tmuxSpec ::
  Show s =>
  DeepPrisms e RpcError =>
  ReportError e =>
  Default s =>
  TestConfig ->
  s ->
  Ribo s e () ->
  IO ()
tmuxSpec conf env specThunk =
  Chiasma.tmuxSpec run
  where
    run api = guiSpec conf api env (withTmux specThunk api)

tmuxSpec' ::
  Show s =>
  DeepPrisms e RpcError =>
  ReportError e =>
  Default s =>
  TmuxTestConf ->
  TestConfig ->
  s ->
  Ribo s e () ->
  IO ()
tmuxSpec' tmuxConf conf env specThunk =
  Chiasma.tmuxSpec' tmuxConf run
  where
    run api = guiSpec conf api env (withTmux specThunk api)

tmuxSpecDef ::
  Show s =>
  DeepPrisms e RpcError =>
  ReportError e =>
  Default s =>
  Ribo s e () ->
  IO ()
tmuxSpecDef =
  tmuxSpec def def

tmuxGuiSpec ::
  DeepPrisms e RpcError =>
  ReportError e =>
  Default s =>
  TestConfig ->
  s ->
  Ribo s e () ->
  IO ()
tmuxGuiSpec conf env specThunk =
  Chiasma.tmuxGuiSpec run
  where
    run api = guiSpec conf api env (withTmux specThunk api)

tmuxGuiSpecDef ::
  DeepPrisms e RpcError =>
  ReportError e =>
  Default s =>
  Ribo s e () ->
  IO ()
tmuxGuiSpecDef =
  tmuxGuiSpec def def

withTmuxInt ::
  NvimE e m =>
  MonadIO m =>
  RpcHandler e () m =>
  Text ->
  DeepPrisms e RpcError =>
  m () ->
  TmuxNative ->
  m ()
withTmuxInt name thunk (TmuxNative (Just socket)) = do
  () <- vimSetVar (name <> "_tmux_socket") (toMsgpack socket)
  thunk
withTmuxInt _ _ _ =
  throwString "no socket in test tmux"

runTmuxWithPlugin ::
  RpcHandler e () m =>
  ReportError e =>
  TmuxNative ->
  TestConfig ->
  Plugin env ->
  m () ->
  IO ()
runTmuxWithPlugin api conf plugin thunk = do
  socketDir <- tempDir "gui-spec"
  withTempDirectory socketDir "spec" runProc
  where
    runProc temp =
      withProcess (testNvimProcessConfig conf) (run temp)
    run temp prc = do
      nvimConf <- Internal.newConfig (pure Nothing) (pure ())
      bracket (acquire prc nvimConf temp) release (const $ runTest conf nvimConf thunk)
    acquire prc nvimConf temp = do
      socket <- startNvimInTmux api temp
      runPlugin socket socket [wrapPlugin plugin] nvimConf <* sleep 0.5
    release transitions =
      tryPutMVar transitions Internal.Quit *> sleep 0.5

tmuxIntegrationSpecDef ::
  NvimE e m =>
  MonadIO m =>
  RpcHandler e () m =>
  ReportError e =>
  Text ->
  Plugin env ->
  m () ->
  IO ()
tmuxIntegrationSpecDef name plugin specThunk =
  Chiasma.tmuxGuiSpec run
  where
    run api =
      runTmuxWithPlugin api def plugin (withTmuxInt name specThunk api)
