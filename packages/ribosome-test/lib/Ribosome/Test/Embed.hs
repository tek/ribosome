module Ribosome.Test.Embed where

import Chiasma.Test.Tmux (withProcessWait)
import Control.Concurrent.Async.Lifted (async, cancel, race)
import Control.Concurrent.Lifted (fork)
import Control.Exception.Lifted (bracket, try)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Map.Strict as Map (fromList, toList, union)
import Hedgehog (TestT)
import Hedgehog.Internal.Property (mkTestT, runTestT)
import Neovim (Neovim, Object)
import Neovim.API.Text (vim_command)
import qualified Neovim.Context.Internal as Internal (
  Config,
  Neovim(Neovim),
  StateTransition(Failure, InitSuccess, Quit),
  globalFunctionMap,
  mkFunctionMap,
  newConfig,
  pluginSettings,
  retypeConfig,
  transitionTo,
  )
import Neovim.Main (standalone)
import Neovim.Plugin (Plugin(Plugin), startPluginThreads)
import Neovim.Plugin.Internal (NeovimPlugin, wrapPlugin)
import Neovim.RPC.Common (RPCConfig, newRPCConfig)
import Neovim.RPC.EventHandler (runEventHandler)
import Neovim.RPC.SocketReader (runSocketReader)
import System.Directory (makeAbsolute)
import System.Exit (ExitCode)
import System.Log.Logger (Priority(ERROR), setLevel, updateGlobalLogger)
import qualified System.Posix.Signals as Signal (killProcess, signalProcess)
import System.Process (getPid)
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
  startProcess,
  stopProcess,
  unsafeProcessHandle,
  )

import Ribosome.Api.Option (rtpCat)
import Ribosome.Control.Exception (tryAny)
import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Control.Ribosome (Ribosome(Ribosome), newRibosomeTMVar)
import qualified Ribosome.Data.ErrorReport as ErrorReport (ErrorReport(..))
import Ribosome.Error.Report.Class (ReportError(errorReport))
import Ribosome.Nvim.Api.IO (vimSetVar)
import Ribosome.Plugin.RpcHandler (RpcHandler(native))
import Ribosome.System.Time (sleep, sleepW)
import Ribosome.Test.Orphans ()

type Runner m = ∀ a . TestConfig -> m a -> m a

newtype Vars =
  Vars (Map Text Object)
  deriving (Eq, Show)
  deriving newtype (Default, Semigroup, Monoid)

-- |left biased
varsUnion :: Vars -> Vars -> Vars
varsUnion (Vars v1) (Vars v2) =
  Vars (Map.union v1 v2)

varsFromList :: [(Text, Object)] -> Vars
varsFromList =
  Vars . Map.fromList

data TestConfig =
  TestConfig {
    tcPluginName :: Text,
    tcExtraRtp :: Text,
    tcLogPath :: FilePath,
    tcTimeout :: Word,
    tcCmdline :: Maybe [Text],
    tcCmdArgs :: [Text],
    tcVariables :: Vars
  }

instance Default TestConfig where
  def = TestConfig "ribosome" "test/u/fixtures/rtp" "test/u/temp/log" 10 def def def

defaultTestConfigWith :: Text -> Vars -> TestConfig
defaultTestConfigWith name vars =
  def { tcPluginName = name, tcVariables = vars }

defaultTestConfig :: Text -> TestConfig
defaultTestConfig name = defaultTestConfigWith name def

setVars :: ∀ m e. NvimE e m => Vars -> m ()
setVars (Vars vars) =
  traverse_ set (Map.toList vars)
  where
    set :: (Text, Object) -> m ()
    set =
      uncurry vimSetVar

setupPluginEnv ::
  MonadIO m =>
  NvimE e m =>
  TestConfig ->
  m ()
setupPluginEnv (TestConfig _ rtp _ _ _ _ vars) = do
  absRtp <- liftIO $ makeAbsolute (toString rtp)
  rtpCat (toText absRtp)
  setVars vars

killPid :: Integral a => a -> IO ()
killPid =
  void . tryAny . Signal.signalProcess Signal.killProcess . fromIntegral

killProcess :: Process i o e -> IO ()
killProcess prc = do
  let handle = unsafeProcessHandle prc
  mayPid <- getPid handle
  traverse_ killPid mayPid

testNvimProcessConfig :: TestConfig -> ProcessConfig Handle Handle ()
testNvimProcessConfig TestConfig {..} =
  setStdin createPipe . setStdout createPipe . proc "nvim" . fmap toString $ args <> tcCmdArgs
  where
    args = fromMaybe defaultArgs tcCmdline
    defaultArgs = ["--embed", "-n", "-u", "NONE", "-i", "NONE"]

startHandlers ::
  MonadIO m =>
  Handle ->
  Handle ->
  TestConfig ->
  Internal.Config RPCConfig ->
  m (IO ())
startHandlers stdoutHandle stdinHandle TestConfig{} nvimConf = do
  socketReader <- liftIO (run runSocketReader stdoutHandle)
  eventHandler <- liftIO (run runEventHandler stdinHandle)
  atomically $ putTMVar (Internal.globalFunctionMap nvimConf) (Internal.mkFunctionMap [])
  let stopEventHandlers = traverse_ @[] cancel [socketReader, eventHandler]
  return stopEventHandlers
  where
    run runner hand = async . void $ runner hand emptyConf
    emptyConf = nvimConf { Internal.pluginSettings = Nothing }

startStdioHandlers ::
  MonadIO m =>
  NvimProc ->
  TestConfig ->
  Internal.Config RPCConfig ->
  m (IO ())
startStdioHandlers prc =
  startHandlers (getStdout prc) (getStdin prc)

runNeovimThunk :: Internal.Config e -> Neovim e a -> IO a
runNeovimThunk cfg (Internal.Neovim thunk) =
  runReaderT (runResourceT thunk) cfg

type NvimProc = Process Handle Handle ()

waitQuit :: NvimProc -> IO (Maybe ExitCode)
waitQuit prc =
  wait 30
  where
    wait :: Int -> IO (Maybe ExitCode)
    wait 0 = return Nothing
    wait count = do
      code <- getExitCode prc
      case code of
        Just a -> return $ Just a
        Nothing -> do
          sleep 0.1
          wait $ count - 1

quitNvim :: Internal.Config e -> NvimProc -> IO ()
quitNvim testCfg prc = do
  quitThread <- async $ runNeovimThunk testCfg quit
  result <- waitQuit prc
  case result of
    Just _ -> return ()
    Nothing -> killProcess prc
  cancel quitThread
  where
    quit = vim_command "qall!"

shutdownNvim :: Internal.Config e -> NvimProc -> IO () -> IO ()
shutdownNvim _ prc stopEventHandlers = do
  stopEventHandlers
  killProcess prc

runTest ::
  MonadIO m =>
  MonadFail m =>
  ReportError e =>
  RpcHandler e env n =>
  MonadBaseControl IO m =>
  TestConfig ->
  Internal.Config env ->
  n a ->
  m a
runTest TestConfig{..} testCfg thunk = do
  race (sleepW tcTimeout) (liftIO (runNeovimThunk testCfg (runExceptT $ native thunk))) >>= \case
    Right (Right a) -> pure a
    Right (Left e) -> fail . toString . unlines . ErrorReport._log . errorReport $ e
    Left _ -> fail $ "test exceeded timeout of " <> show tcTimeout <> " seconds"

runEmbeddedNvim ::
  MonadIO m =>
  MonadFail m =>
  MonadBaseControl IO m =>
  RpcHandler e env n =>
  ReportError e =>
  TestConfig ->
  env ->
  n a ->
  NvimProc ->
  m a
runEmbeddedNvim conf ribo thunk prc = do
  nvimConf <- liftIO (Internal.newConfig (pure Nothing) newRPCConfig)
  let testCfg = Internal.retypeConfig ribo nvimConf
  bracket (startStdioHandlers prc conf nvimConf) (liftIO . shutdownNvim testCfg prc) (const $ runTest conf testCfg thunk)

withProcessTerm ::
  MonadIO m =>
  MonadBaseControl IO m =>
  ProcessConfig stdin stdout stderr ->
  (Process stdin stdout stderr -> m a) ->
  m a
withProcessTerm config =
  bracket (startProcess config) (try @_ @SomeException . stopProcess)

runEmbedded ::
  MonadIO m =>
  MonadFail m =>
  MonadBaseControl IO m =>
  RpcHandler e env n =>
  ReportError e =>
  TestConfig ->
  env ->
  n a ->
  m a
runEmbedded conf ribo thunk = do
  let pc = testNvimProcessConfig conf
  withProcessWait pc $ runEmbeddedNvim conf ribo thunk

unsafeEmbeddedSpec ::
  MonadIO m =>
  MonadFail m =>
  MonadBaseControl IO m =>
  RpcHandler e env n =>
  ReportError e =>
  Runner n ->
  TestConfig ->
  env ->
  n a ->
  m a
unsafeEmbeddedSpec runner conf s spec =
  runEmbedded conf s $ runner conf spec

unsafeEmbeddedSpecR ::
  MonadIO m =>
  MonadFail m =>
  ReportError e =>
  MonadBaseControl IO m =>
  RpcHandler e (Ribosome env) n =>
  Runner n ->
  TestConfig ->
  env ->
  n a ->
  m a
unsafeEmbeddedSpecR runner conf env spec = do
  tv <- newRibosomeTMVar env
  let ribo = Ribosome (tcPluginName conf) tv
  unsafeEmbeddedSpec runner conf ribo spec

runPlugin ::
  MonadIO m =>
  MonadBaseControl IO m =>
  Handle ->
  Handle ->
  [Neovim () NeovimPlugin] ->
  Internal.Config c ->
  m (MVar Internal.StateTransition)
runPlugin evHandlerHandle sockreaderHandle plugins baseConf = do
  liftIO (updateGlobalLogger "Neovim.Plugin" (setLevel ERROR))
  rpcConf <- liftIO newRPCConfig
  let conf = Internal.retypeConfig rpcConf baseConf
  ehTid <- (fmap void . async . liftIO) (runEventHandler evHandlerHandle conf { Internal.pluginSettings = Nothing })
  srTid <- (fmap void . async . liftIO) (runSocketReader sockreaderHandle conf)
  void $ fork $ liftIO $ startPluginThreads (Internal.retypeConfig () baseConf) plugins >>= \case
    Left e -> do
      putMVar (Internal.transitionTo conf) $ Internal.Failure e
      standalone [ehTid, srTid] conf
    Right (funMapEntries, pluginTids) -> do
      atomically $ putTMVar (Internal.globalFunctionMap conf) (Internal.mkFunctionMap funMapEntries)
      putMVar (Internal.transitionTo conf) Internal.InitSuccess
      standalone (srTid : ehTid : pluginTids) conf
  return (Internal.transitionTo conf)

inTestT ::
  ∀ n m a .
  TestT n a ->
  (∀ x . n x -> m x) ->
  TestT m a
inTestT ma f =
  mkTestT (f (runTestT ma))

integrationSpec ::
  ∀ n m e env a .
  NvimE e n =>
  MonadIO n =>
  MonadIO m =>
  MonadFail m =>
  ReportError e =>
  RpcHandler e env n =>
  MonadBaseControl IO m =>
  TestConfig ->
  Plugin env ->
  TestT n a ->
  TestT m a
integrationSpec conf plugin@(Plugin env _) thunk =
  inTestT thunk \ na ->
    withProcessTerm (testNvimProcessConfig conf) (run na)
  where
    run :: ∀ x . n x -> Process Handle Handle () -> m x
    run na prc = do
      nvimConf <- liftIO (Internal.newConfig (pure Nothing) (pure env))
      bracket (acquire prc nvimConf) release (const $ runTest conf nvimConf (setupPluginEnv conf *> na))
    acquire prc nvimConf =
      liftIO (runPlugin (getStdin prc) (getStdout prc) [wrapPlugin plugin] nvimConf <* sleep 0.5)
    release transitions =
      tryPutMVar transitions Internal.Quit *> sleep 0.5

integrationSpecDef ::
  NvimE e n =>
  MonadIO m =>
  MonadIO n =>
  MonadFail m =>
  ReportError e =>
  RpcHandler e env n =>
  MonadBaseControl IO m =>
  Plugin env ->
  TestT n a ->
  TestT m a
integrationSpecDef =
  integrationSpec def
