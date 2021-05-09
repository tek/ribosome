module Ribosome.Test.Tmux where

import Chiasma.Command.Pane (sendKeys)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxId (PaneId(PaneId))
import Chiasma.Monad.Stream (runTmux)
import Chiasma.Native.Api (TmuxNative(TmuxNative))
import Chiasma.Test.Tmux (TmuxTestConf, withSystemTempDir)
import qualified Chiasma.Test.Tmux as Chiasma (tmuxGuiSpec, tmuxSpec, tmuxSpec')
import Control.Exception.Lifted (bracket)
import Data.DeepPrisms (DeepPrisms)
import Hedgehog (TestT)
import Hedgehog.Internal.Property (failWith)
import qualified Neovim.Context.Internal as Internal (StateTransition(Quit), newConfig, retypeConfig)
import Neovim.Plugin (Plugin(Plugin))
import Neovim.Plugin.Internal (wrapPlugin)
import Neovim.RPC.Common (SocketType(UnixSocket), createHandle, newRPCConfig)
import System.Directory (doesPathExist)
import System.FilePath ((</>))

import Ribosome.Config.Setting (updateSetting)
import Ribosome.Config.Settings (tmuxSocket)
import Ribosome.Control.Concurrent.Wait (waitIOPredDef)
import Ribosome.Control.Exception (catchAny, tryAny)
import Ribosome.Control.Monad.Ribo (MonadRibo, Nvim, NvimE, Ribo)
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

type RiboTesting e env m n =
  (
    NvimE e n,
    MonadRibo n,
    MonadIO m,
    MonadFail m,
    ReportError e,
    RpcHandler e env n,
    MonadBaseControl IO m
  )

runSocketNvimHs ::
  RiboTesting e env m n =>
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
  RiboTesting e env m n =>
  TmuxNative ->
  FilePath ->
  TestConfig ->
  env ->
  n a ->
  m a
runGui api temp conf ribo specThunk =
  runSocketNvimHs conf ribo specThunk =<< liftIO (startNvimInTmux api temp)

unsafeGuiSpec ::
  RiboTesting e env m n =>
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
  RiboTesting e (Ribosome env) m n =>
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
  RiboTesting e (Ribosome env) m n =>
  TestConfig ->
  TmuxNative ->
  env ->
  n a ->
  m a
guiSpec conf api env specThunk = do
  withSystemTempDir run
  where
    run tempdir =
      unsafeGuiSpecR api tempdir uSpec conf env specThunk

withTmux ::
  Nvim m =>
  MonadRibo m =>
  MonadDeepError e RpcError m =>
  m a ->
  TmuxNative ->
  m a
withTmux thunk (TmuxNative (Just socket)) =
  updateSetting tmuxSocket socket *> thunk
withTmux _ _ =
  throwText "no socket in test tmux"

tmuxSpec ::
  RiboTesting e (Ribosome env) m n =>
  TestConfig ->
  env ->
  TestT n a ->
  TestT m a
tmuxSpec conf env specThunk =
  inTestT specThunk \ th ->
    Chiasma.tmuxSpec \ api -> guiSpec conf api env (withTmux th api)

tmuxSpec' ::
  RiboTesting e (Ribosome env) m n =>
  TmuxTestConf ->
  TestConfig ->
  env ->
  TestT n a ->
  TestT m a
tmuxSpec' tmuxConf conf env specThunk =
  inTestT specThunk \ th ->
    Chiasma.tmuxSpec' tmuxConf \ api ->
      guiSpec conf api env (withTmux th api)

tmuxSpecDef ::
  Default s =>
  ReportError e =>
  DeepPrisms e RpcError =>
  TestT (Ribo s e) () ->
  UnitTest
tmuxSpecDef =
  tmuxSpec def def

tmuxGuiSpec ::
  RiboTesting e (Ribosome env) m n =>
  TestConfig ->
  env ->
  TestT n a ->
  TestT m a
tmuxGuiSpec conf env specThunk =
  inTestT specThunk \ th ->
    Chiasma.tmuxGuiSpec \ api ->
      guiSpec conf api env (withTmux th api)

tmuxGuiSpecDef ::
  Default env =>
  RiboTesting e (Ribosome env) m n =>
  TestT n a ->
  TestT m a
tmuxGuiSpecDef =
  tmuxGuiSpec def def

withTmuxInt ::
  NvimE e m =>
  MonadIO m =>
  Text ->
  m a ->
  TmuxNative ->
  m a
withTmuxInt name thunk (TmuxNative (Just socket)) = do
  () <- vimSetVar (name <> "_tmux_socket") (toMsgpack socket)
  thunk
withTmuxInt _ _ _ =
  throwText "no socket in test tmux"

runTmuxWithPlugin ::
  RiboTesting e env m n =>
  TmuxNative ->
  TestConfig ->
  Plugin env ->
  TestT n a ->
  TestT m a
runTmuxWithPlugin api conf plugin@(Plugin env _) thunk = do
  withSystemTempDir runProc
  where
    runProc temp =
      either logError pure =<< (tryAny (withProcessTerm (testNvimProcessConfig conf) (run temp)))
    run temp prc = do
      nvimConf <- liftIO (Internal.newConfig (pure Nothing) (pure env))
      bracket (acquire prc nvimConf temp) release (const $ inTestT thunk (runTest conf nvimConf))
    acquire _ nvimConf temp = do
      socket <- liftIO (startNvimInTmux api temp)
      runPlugin socket socket [wrapPlugin plugin] nvimConf <* sleep 0.5
    release transitions =
      tryPutMVar transitions Internal.Quit *> sleep 0.5
    logError (SomeException e) =
      failWith Nothing ("runTmuxWithPlugin: nvim process failed with: " <> show e)

tmuxIntegrationSpecDef ::
  RiboTesting e env m n =>
  Text ->
  Plugin env ->
  TestT n a ->
  TestT m a
tmuxIntegrationSpecDef name plugin specThunk =
  Chiasma.tmuxGuiSpec run
  where
    run api =
      runTmuxWithPlugin api def plugin (inTestT specThunk \ th -> withTmuxInt name th api)
