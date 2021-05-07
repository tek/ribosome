module Ribosome.Test.Tmux where

import Hedgehog (TestT)
import Chiasma.Command.Pane (sendKeys)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxId (PaneId(PaneId))
import Chiasma.Monad.Stream (runTmux)
import Chiasma.Native.Api (TmuxNative(TmuxNative))
import Chiasma.Test.Tmux (TmuxTestConf, withSystemTempDir)
import qualified Chiasma.Test.Tmux as Chiasma (tmuxGuiSpec, tmuxSpec, tmuxSpec')
import Control.Exception.Lifted (bracket)
import Data.DeepPrisms (DeepPrisms)
import qualified Data.Text.IO as Text
import qualified Neovim.Context.Internal as Internal (
  StateTransition(Quit),
  newConfig,
  retypeConfig,
  )
import Neovim.Plugin (Plugin(Plugin))
import Neovim.Plugin.Internal (wrapPlugin)
import Neovim.RPC.Common (SocketType(UnixSocket), createHandle, newRPCConfig)
import System.Directory (doesPathExist)
import System.FilePath ((</>))

import Ribosome.Config.Setting (updateSetting)
import Ribosome.Config.Settings (tmuxSocket)
import Ribosome.Control.Concurrent.Wait (waitIOPredDef)
import Ribosome.Control.Exception (catchAny, tryAny)
import Ribosome.Control.Monad.Ribo (NvimE, Ribo)
import Ribosome.Control.Ribosome (Ribosome(Ribosome), newRibosomeTMVar)
import Ribosome.Error.Report.Class (ReportError)
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Nvim.Api.IO (vimSetVar)
import Ribosome.Nvim.Api.RpcCall (RpcError)
import Ribosome.Plugin.RpcHandler (RpcHandler)
import Ribosome.System.Time (sleep)
import Ribosome.Test.Embed (
  Runner,
  TestConfig(..),
  inTestT,
  runPlugin,
  runTest,
  startHandlers,
  testNvimProcessConfig,
  withProcessTerm,
  )
import Ribosome.Test.Orphans ()
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Unit (uSpec)

runSocketNvimHs ::
  MonadIO m =>
  MonadFail m =>
  ReportError e =>
  RpcHandler e env n =>
  MonadBaseControl IO m =>
  TestConfig ->
  env ->
  n a ->
  Handle ->
  m a
runSocketNvimHs conf ribo specThunk socket = do
  nvimConf <- liftIO (Internal.newConfig (pure Nothing) newRPCConfig)
  let testCfg = Internal.retypeConfig ribo nvimConf
  bracket (startHandlers socket socket conf nvimConf) liftIO (const $ runTest conf testCfg specThunk)

externalNvimCmdline :: FilePath -> Text
externalNvimCmdline socket =
  "nvim --listen " <> toText socket <> " -n -u NONE -i NONE"

startNvimInTmux ::
  TmuxNative ->
  FilePath ->
  IO Handle
startNvimInTmux api temp = do
  void $ runExceptT @TmuxError $ runTmux api $ sendKeys (PaneId 0) [externalNvimCmdline socket]
  _ <- waitIOPredDef (pure socket) doesPathExist
  catchAny err (createHandle (UnixSocket socket))
  where
    socket =
      temp </> "nvim-socket"
    err (SomeException e) =
      fail ("startNvimInTmux: createHandle failed: " <> show e)

runGui ::
  MonadIO m =>
  MonadFail m =>
  ReportError e =>
  RpcHandler e env n =>
  MonadBaseControl IO m =>
  TmuxNative ->
  FilePath ->
  TestConfig ->
  env ->
  n a ->
  m a
runGui api temp conf ribo specThunk =
  runSocketNvimHs conf ribo specThunk =<< liftIO (startNvimInTmux api temp)

unsafeGuiSpec ::
  MonadIO m =>
  MonadFail m =>
  ReportError e =>
  RpcHandler e env n =>
  MonadBaseControl IO m =>
  TmuxNative ->
  FilePath ->
  Runner n ->
  TestConfig ->
  env ->
  n a ->
  m a
unsafeGuiSpec api temp runner conf s specThunk =
  runGui api temp conf s $ runner conf specThunk

unsafeGuiSpecR ::
  MonadIO m =>
  MonadFail m =>
  ReportError e =>
  MonadBaseControl IO m =>
  RpcHandler e (Ribosome env) n =>
  TmuxNative ->
  FilePath ->
  Runner n ->
  TestConfig ->
  env ->
  n a ->
  m a
unsafeGuiSpecR api temp runner conf s specThunk = do
  tv <- newRibosomeTMVar s
  let ribo = Ribosome (tcPluginName conf) tv
  unsafeGuiSpec api temp runner conf ribo specThunk

guiSpec ::
  MonadIO m =>
  MonadFail m =>
  ReportError e =>
  MonadBaseControl IO m =>
  DeepPrisms e RpcError =>
  TestConfig ->
  TmuxNative ->
  s ->
  Ribo s e a ->
  m a
guiSpec conf api env specThunk = do
  withSystemTempDir run
  where
    run tempdir =
      unsafeGuiSpecR api tempdir uSpec conf env specThunk

withTmux ::
  DeepPrisms e RpcError =>
  Ribo s e a ->
  TmuxNative ->
  Ribo s e a
withTmux thunk (TmuxNative (Just socket)) =
  updateSetting tmuxSocket socket *> thunk
withTmux _ _ =
  throwText "no socket in test tmux"

tmuxSpec ::
  DeepPrisms e RpcError =>
  ReportError e =>
  TestConfig ->
  s ->
  TestT (Ribo s e) () ->
  UnitTest
tmuxSpec conf env specThunk =
  inTestT specThunk \ th ->
    Chiasma.tmuxSpec \ api -> guiSpec conf api env (withTmux th api)

tmuxSpec' ::
  DeepPrisms e RpcError =>
  ReportError e =>
  TmuxTestConf ->
  TestConfig ->
  s ->
  TestT (Ribo s e) () ->
  UnitTest
tmuxSpec' tmuxConf conf env specThunk =
  inTestT specThunk \ th ->
    Chiasma.tmuxSpec' tmuxConf \ api ->
      guiSpec conf api env (withTmux th api)

tmuxSpecDef ::
  DeepPrisms e RpcError =>
  ReportError e =>
  Default s =>
  TestT (Ribo s e) () ->
  UnitTest
tmuxSpecDef =
  tmuxSpec def def

tmuxGuiSpec ::
  DeepPrisms e RpcError =>
  ReportError e =>
  TestConfig ->
  s ->
  Ribo s e () ->
  UnitTest
tmuxGuiSpec conf env specThunk =
  Chiasma.tmuxGuiSpec run
  where
    run api = guiSpec conf api env (withTmux specThunk api)

tmuxGuiSpecDef ::
  DeepPrisms e RpcError =>
  ReportError e =>
  Default s =>
  Ribo s e () ->
  UnitTest
tmuxGuiSpecDef =
  tmuxGuiSpec def def

withTmuxInt ::
  NvimE e m =>
  MonadIO m =>
  Text ->
  m () ->
  TmuxNative ->
  m ()
withTmuxInt name thunk (TmuxNative (Just socket)) = do
  () <- vimSetVar (name <> "_tmux_socket") (toMsgpack socket)
  thunk
withTmuxInt _ _ _ =
  throwText "no socket in test tmux"

runTmuxWithPlugin ::
  MonadIO m =>
  MonadFail m =>
  ReportError e =>
  RpcHandler e env n =>
  MonadBaseControl IO m =>
  TmuxNative ->
  TestConfig ->
  Plugin env ->
  n () ->
  m ()
runTmuxWithPlugin api conf plugin@(Plugin env _) thunk = do
  withSystemTempDir runProc
  where
    runProc temp =
      either logError pure =<< (tryAny (withProcessTerm (testNvimProcessConfig conf) (run temp)))
    run temp prc = do
      nvimConf <- liftIO (Internal.newConfig (pure Nothing) (pure env))
      bracket (acquire prc nvimConf temp) release (const $ runTest conf nvimConf thunk)
    acquire _ nvimConf temp = do
      socket <- liftIO (startNvimInTmux api temp)
      runPlugin socket socket [wrapPlugin plugin] nvimConf <* sleep 0.5
    release transitions =
      tryPutMVar transitions Internal.Quit *> sleep 0.5
    logError (SomeException e) =
      liftIO (Text.hPutStr stderr ("runTmuxWithPlugin: nvim process failed with: " <> show e))

tmuxIntegrationSpecDef ::
  NvimE e n =>
  MonadIO m =>
  MonadIO n =>
  MonadFail m =>
  ReportError e =>
  RpcHandler e env n =>
  MonadBaseControl IO m =>
  Text ->
  Plugin env ->
  n () ->
  m ()
tmuxIntegrationSpecDef name plugin specThunk =
  Chiasma.tmuxGuiSpec run
  where
    run api =
      runTmuxWithPlugin api def plugin (withTmuxInt name specThunk api)
