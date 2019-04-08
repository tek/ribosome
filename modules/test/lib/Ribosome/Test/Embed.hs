module Ribosome.Test.Embed where

import Control.Concurrent (forkIO)
import Control.Monad.DeepError (MonadDeepError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Default (Default(def))
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import GHC.IO.Handle (Handle)
import Neovim (Neovim, Object)
import Neovim.API.String (vim_command)
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
import Neovim.Plugin (Plugin, startPluginThreads)
import Neovim.Plugin.Internal (NeovimPlugin, wrapPlugin)
import Neovim.RPC.Common (RPCConfig, newRPCConfig)
import Neovim.RPC.EventHandler (runEventHandler)
import Neovim.RPC.SocketReader (runSocketReader)
import System.Directory (makeAbsolute)
import System.Exit (ExitCode)
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
  unsafeProcessHandle,
  withProcess,
  )
import UnliftIO.Async (async, cancel, race)
import UnliftIO.Exception (bracket, tryAny)
import UnliftIO.MVar (MVar, putMVar, tryPutMVar)
import UnliftIO.STM (atomically, putTMVar)

import Ribosome.Api.Option (rtpCat)
import Ribosome.Control.Monad.Ribo (Nvim, NvimE)
import Ribosome.Control.Ribosome (Ribosome(Ribosome), newRibosomeTVar)
import qualified Ribosome.Data.ErrorReport as ErrorReport (ErrorReport(..))
import Ribosome.Error.Report.Class (ReportError(errorReport))
import Ribosome.Nvim.Api.IO (vimSetVar)
import Ribosome.Nvim.Api.RpcCall (RpcError)
import Ribosome.Plugin (RpcHandler(native))
import Ribosome.System.Time (sleep, sleepW)

type Runner m = TestConfig -> m () -> m ()

newtype Vars = Vars [(String, Object)]

data TestConfig =
  TestConfig {
    tcPluginName :: String,
    tcExtraRtp :: String,
    tcLogPath :: FilePath,
    tcTimeout :: Word,
    tcCmdline :: Maybe [String],
    tcCmdArgs :: [String],
    tcVariables :: Vars
  }

instance Default TestConfig where
  def = TestConfig "ribosome" "test/f/fixtures/rtp" "test/f/temp/log" 10 def def (Vars [])

defaultTestConfigWith :: String -> Vars -> TestConfig
defaultTestConfigWith name vars =
  def { tcPluginName = name, tcVariables = vars }

defaultTestConfig :: String -> TestConfig
defaultTestConfig name = defaultTestConfigWith name (Vars [])

setVars :: ∀ m e. (Nvim m, MonadDeepError e RpcError m) => Vars -> m ()
setVars (Vars vars) =
  traverse_ set vars
  where
    set :: (String, Object) -> m ()
    set = uncurry vimSetVar

setupPluginEnv :: (MonadIO m, NvimE e m) => TestConfig -> m ()
setupPluginEnv (TestConfig _ rtp _ _ _ _ vars) = do
  absRtp <- liftIO $ makeAbsolute rtp
  rtpCat absRtp
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
  setStdin createPipe $ setStdout createPipe $ proc "nvim" $ args ++ tcCmdArgs
  where
    args = fromMaybe defaultArgs tcCmdline
    defaultArgs = ["--embed", "-n", "-u", "NONE", "-i", "NONE"]

startHandlers :: Handle -> Handle -> TestConfig -> Internal.Config RPCConfig -> IO (IO ())
startHandlers stdoutHandle stdinHandle TestConfig{..} nvimConf = do
  socketReader <- run runSocketReader stdoutHandle
  eventHandler <- run runEventHandler stdinHandle
  atomically $ putTMVar (Internal.globalFunctionMap nvimConf) (Internal.mkFunctionMap [])
  let stopEventHandlers = traverse_ cancel [socketReader, eventHandler]
  return stopEventHandlers
  where
    run runner hand = async . void $ runner hand emptyConf
    emptyConf = nvimConf { Internal.pluginSettings = Nothing }

startStdioHandlers :: NvimProc -> TestConfig -> Internal.Config RPCConfig -> IO (IO ())
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
  -- quitNvim testCfg prc

runTest ::
  RpcHandler e env m =>
  ReportError e =>
  TestConfig ->
  Internal.Config env ->
  m () ->
  IO ()
runTest TestConfig{..} testCfg thunk = do
  result <- race (sleepW tcTimeout) (runNeovimThunk testCfg (runExceptT $ native thunk))
  case result of
    Right (Right _) -> return ()
    Right (Left e) -> fail . unlines . ErrorReport._log . errorReport $ e
    Left _ -> fail $ "test exceeded timeout of " ++ show tcTimeout ++ " seconds"

runEmbeddedNvim ::
  RpcHandler e env m =>
  ReportError e =>
  TestConfig ->
  env ->
  m () ->
  NvimProc ->
  IO ()
runEmbeddedNvim conf ribo thunk prc = do
  nvimConf <- Internal.newConfig (pure Nothing) newRPCConfig
  let testCfg = Internal.retypeConfig ribo nvimConf
  bracket (startStdioHandlers prc conf nvimConf) (shutdownNvim testCfg prc) (const $ runTest conf testCfg thunk)

runEmbedded ::
  RpcHandler e env m =>
  ReportError e =>
  TestConfig ->
  env ->
  m () ->
  IO ()
runEmbedded conf ribo thunk = do
  let pc = testNvimProcessConfig conf
  withProcess pc $ runEmbeddedNvim conf ribo thunk

unsafeEmbeddedSpec ::
  RpcHandler e env m =>
  ReportError e =>
  Runner m ->
  TestConfig ->
  env ->
  m () ->
  IO ()
unsafeEmbeddedSpec runner conf s spec =
  runEmbedded conf s $ runner conf spec

unsafeEmbeddedSpecR ::
  RpcHandler e (Ribosome env) m =>
  ReportError e =>
  Runner m ->
  TestConfig ->
  env ->
  m () ->
  IO ()
unsafeEmbeddedSpecR runner conf env spec = do
  tv <- newRibosomeTVar env
  let ribo = Ribosome (tcPluginName conf) tv
  unsafeEmbeddedSpec runner conf ribo spec

runPlugin ::
  Handle ->
  Handle ->
  [Neovim () NeovimPlugin] ->
  Internal.Config c ->
  IO (MVar Internal.StateTransition)
runPlugin evHandlerHandle sockreaderHandle plugins baseConf = do
  rpcConf <- newRPCConfig
  let conf = Internal.retypeConfig rpcConf baseConf
  ehTid <- async $ runEventHandler evHandlerHandle conf { Internal.pluginSettings = Nothing }
  srTid <- async $ runSocketReader sockreaderHandle conf
  void $ forkIO $ startPluginThreads (Internal.retypeConfig () baseConf) plugins >>= \case
    Left e -> do
      putMVar (Internal.transitionTo conf) $ Internal.Failure e
      standalone [ehTid, srTid] conf
    Right (funMapEntries, pluginTids) -> do
      atomically $ putTMVar (Internal.globalFunctionMap conf) (Internal.mkFunctionMap funMapEntries)
      putMVar (Internal.transitionTo conf) Internal.InitSuccess
      standalone (srTid:ehTid:pluginTids) conf
  return (Internal.transitionTo conf)

runEmbeddedWithPlugin ::
  RpcHandler e () m =>
  ReportError e =>
  TestConfig ->
  Plugin env ->
  m () ->
  IO ()
runEmbeddedWithPlugin conf plugin thunk =
  withProcess (testNvimProcessConfig conf) run
  where
    run prc = do
      nvimConf <- Internal.newConfig (pure Nothing) (pure ())
      bracket (acquire prc nvimConf) release (const $ runTest conf nvimConf thunk)
    acquire prc nvimConf =
      runPlugin (getStdin prc) (getStdout prc) [wrapPlugin plugin] nvimConf <* sleep 0.5
    release transitions =
      tryPutMVar transitions Internal.Quit *> sleep 0.5

integrationSpecDef ::
  RpcHandler e () m =>
  ReportError e =>
  Plugin env ->
  m () ->
  IO ()
integrationSpecDef =
  runEmbeddedWithPlugin def
