module Integration.Test.PluginTest where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Exon (exon)
import Hedgehog.Internal.Property (failWith)
import Log (Severity (Trace))
import Path (Abs, Dir, Path, reldir, relfile, toFilePath, (</>))
import Path.IO (copyDirRecur)
import qualified Polysemy.Conc as Conc
import qualified Polysemy.Test as Test
import Polysemy.Test (TestError (TestError), UnitTest, assertEq, liftH)
import qualified Polysemy.Time as Time
import Polysemy.Time (MilliSeconds (MilliSeconds), Minutes (Minutes))
import Ribosome.Embed (HandlerDeps, withPluginEmbed)
import Ribosome.Host.Api.Effect (nvimCallFunction)
import Ribosome.Host.Data.HostConfig (hostLog, logLevelStderr)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (interpretEmbedExtra)
import Ribosome.Host.IOStack (IOStack)
import Ribosome.Host.Interpreter.Process.Embed (interpretProcessCerealNvimEmbed, nvimArgs)
import Ribosome.Host.Run (interpretRpcStack)
import Ribosome.Host.Test.Data.TestConfig (host)
import Ribosome.Host.Test.Run (TestConfStack, TestIOStack, runTestConf)
import Ribosome.Interpreter.NvimPlugin (noHandlers)
import Ribosome.Interpreter.Scratch (interpretScratch)
import Ribosome.Interpreter.Settings (interpretSettingsRpc)
import Ribosome.Interpreter.UserError (interpretUserErrorPrefixed)
import System.Environment (lookupEnv)
import System.Process.Typed (ProcessConfig, proc)

import Integration ()

nvimProc ::
  Path Abs Dir ->
  ProcessConfig () () ()
nvimProc path =
  proc "nvim" (nvimArgs <> ["--headless", "--cmd", [exon|set rtp+=#{toFilePath path}|]])

interpretTestPluginEmbed ::
  Member Log r =>
  Members IOStack r =>
  Path Abs Dir ->
  InterpretersFor HandlerDeps r
interpretTestPluginEmbed target =
  runReader "test" .
  interpretUserErrorPrefixed .
  interpretProcessCerealNvimEmbed Nothing (Just (nvimProc target)) .
  interpretEmbedExtra .
  interpretRpcStack .
  interpretSettingsRpc .
  interpretScratch

testPlugin ::
  Members TestIOStack r =>
  Members TestConfStack r =>
  Text ->
  Sem r ()
testPlugin riboRoot = do
  source <- Test.fixturePath [reldir|plugin|]
  target <- Test.tempDir [reldir|plugin|]
  embed (copyDirRecur source target)
  let flake = toFilePath (target </> [relfile|flake.nix|])
  old <- embed (Text.readFile flake)
  embed (Text.writeFile flake (Text.replace "RIBOSOME" (toText riboRoot) old))
  interpretTestPluginEmbed target $ noHandlers $ withPluginEmbed "test" do
      resumeHoistError @RpcError @Rpc (TestError . show @Text) do
        Conc.timeout_ (liftH (failWith Nothing "RPC function did not appear")) (Minutes 2) do
          Time.while (MilliSeconds 500) (resumeAs @RpcError @Rpc True (False <$ nvimCallFunction @Int "Test" []))
        assertEq (5 :: Int) =<< nvimCallFunction "Test" []

test_plugin :: UnitTest
test_plugin =
  runTestConf def { host = def { hostLog = def { logLevelStderr = Trace } } } do
    traverse_ (testPlugin . toText) =<< embed (lookupEnv "RIBOSOME_ROOT")
