module Integration.Test.PluginTest where

import qualified Conc
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Exon (exon)
import Hedgehog.Internal.Property (failWith)
import Log (Severity (Trace))
import Path (Abs, Dir, Path, reldir, relfile, toFilePath, (</>))
import Path.IO (copyDirRecur)
import Polysemy.Chronos (ChronosTime)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assertEq, liftH)
import qualified Polysemy.Time as Time
import Polysemy.Time (MilliSeconds (MilliSeconds), Minutes (Minutes))
import Ribosome.Data.PluginName (PluginName (PluginName))
import Ribosome.Embed (HandlerEffects, embedPlugin)
import Ribosome.Host.Api.Data (nvimCallFunction)
import Ribosome.Host.Data.HostConfig (LogConfig, hostLog, logLevelStderr)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (interpretEmbedExtra)
import Ribosome.Host.IOStack (IOStack)
import Ribosome.Host.Interpreter.Handlers (interpretHandlersNull, noHandlers)
import Ribosome.Host.Interpreter.Process.Embed (interpretProcessCerealNvimEmbed, nvimArgs)
import Ribosome.Host.Run (interpretRpcStack)
import Ribosome.Host.Test.Data.TestConfig (host)
import Ribosome.Host.Test.Run (TestIOStack, TestStack, runTestConf)
import Ribosome.Interpreter.Scratch (interpretScratch)
import Ribosome.Interpreter.Settings (interpretSettingsRpc)
import Ribosome.Interpreter.UserError (interpretUserErrorPrefixed)
import Ribosome.Interpreter.VariableWatcher (interpretVariableWatcherNull)
import Ribosome.Test.Error (resumeTestError)
import System.Environment (lookupEnv)
import System.Process.Typed (ProcessConfig, proc)

import Integration ()

nvimProc ::
  Path Abs Dir ->
  ProcessConfig () () ()
nvimProc path =
  proc "nvim" (nvimArgs <> ["--headless", "--cmd", [exon|set rtp+=#{toFilePath path}|]])

interpretTestPluginEmbed ::
  Members [Reader PluginName, Reader LogConfig, Log] r =>
  Members IOStack r =>
  Path Abs Dir ->
  InterpretersFor HandlerEffects r
interpretTestPluginEmbed target =
  interpretUserErrorPrefixed .
  interpretProcessCerealNvimEmbed Nothing (Just (nvimProc target)) .
  interpretEmbedExtra .
  interpretRpcStack .
  interpretHandlersNull .
  interpretVariableWatcherNull .
  interpretSettingsRpc .
  interpretScratch

waitForFunction ::
  Members TestIOStack r =>
  Members [Rpc !! RpcError, ChronosTime] r =>
  Sem r ()
waitForFunction =
  resumeTestError @Rpc @RpcError do
    Conc.timeout_ (liftH (failWith Nothing "RPC function did not appear")) (Minutes 5) do
      Time.while (MilliSeconds 500) (resumeAs @RpcError @Rpc True (False <$ nvimCallFunction @Int "Test" []))
    assertEq (5 :: Int) =<< nvimCallFunction "Test" []

testPlugin ::
  Text ->
  Sem TestStack ()
testPlugin riboRoot = do
  source <- Test.fixturePath [reldir|plugin|]
  target <- Test.tempDir [reldir|plugin|]
  embed (copyDirRecur source target)
  let flake = toFilePath (target </> [relfile|flake.nix|])
  old <- embed (Text.readFile flake)
  embed (Text.writeFile flake (Text.replace "RIBOSOME" (toText riboRoot) old))
  runReader (PluginName "integration") $ interpretTestPluginEmbed target $ noHandlers $ embedPlugin do
    waitForFunction

test_plugin :: UnitTest
test_plugin =
  runTestConf def { host = def { hostLog = def { logLevelStderr = Trace } } } do
    traverse_ (testPlugin . toText) =<< embed (lookupEnv "RIBOSOME_ROOT")
