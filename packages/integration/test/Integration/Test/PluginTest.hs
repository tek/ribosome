module Integration.Test.PluginTest where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Exon (exon)
import Hedgehog.Internal.Property (failWith)
import Log (Severity (Debug))
import Path (Abs, Dir, Path, reldir, relfile, toFilePath, (</>))
import Path.IO (copyDirRecur)
import Polysemy.Chronos (interpretTimeChronos)
import qualified Polysemy.Conc as Conc
import Polysemy.Conc (interpretRace)
import Polysemy.Log (Severity (Warn), interpretLogStdoutLevelConc)
import qualified Polysemy.Test as Test
import Polysemy.Test (Hedgehog, Test, TestError (TestError), UnitTest, assertEq, liftH, runTestAuto)
import qualified Polysemy.Time as Time
import Polysemy.Time (MilliSeconds (MilliSeconds), Minutes (Minutes))
import Ribosome.Host.Api.Effect (nvimCallFunction)
import Ribosome.Host.Data.BootError (unBootError)
import Ribosome.Host.Data.HostConfig (log, logLevelStderr)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (basicCliArgs, interpretCoreDeps, interpretHostEmbedCore)
import Ribosome.Host.Interpreter.Handlers (interpretHandlersNull)
import Ribosome.Host.Interpreter.Host (withHost)
import Ribosome.Host.Interpreter.UserError (interpretUserErrorInfo)
import System.Environment (lookupEnv)
import System.Process.Typed (ProcessConfig, proc)

conf ::
  Path Abs Dir ->
  ProcessConfig () () ()
conf path =
  proc "nvim" (basicCliArgs <> ["--headless", "--cmd", [exon|set rtp+=#{toFilePath path}|]])

testPlugin ::
  Members [Test, Hedgehog IO, Error TestError, Resource, Embed IO, Final IO] r =>
  Text ->
  Sem r ()
testPlugin riboRoot =
  asyncToIOFinal $
  interpretRace $
  interpretTimeChronos $
  interpretUserErrorInfo $
  interpretLogStdoutLevelConc (Just Warn) $
  mapError (TestError . unBootError) do
    source <- Test.fixturePath [reldir|plugin|]
    target <- Test.tempDir [reldir|plugin|]
    embed (copyDirRecur source target)
    let flake = toFilePath (target </> [relfile|flake.nix|])
    old <- embed (Text.readFile flake)
    embed (Text.writeFile flake (Text.replace "RIBOSOME" (toText riboRoot) old))
    interpretCoreDeps def { log = def { logLevelStderr = Debug } } $
      interpretHostEmbedCore Nothing (Just (conf target)) $
      interpretHandlersNull $
      withHost do
        resumeHoistError @RpcError @Rpc (TestError . show @Text) do
          Conc.timeout_ (liftH (failWith Nothing "RPC function did not appear")) (Minutes 2) do
            Time.while (MilliSeconds 500) (resumeAs @RpcError True (False <$ nvimCallFunction @Int "Test" []))
          assertEq (5 :: Int) =<< nvimCallFunction "Test" []

test_plugin :: UnitTest
test_plugin =
  runTestAuto do
    traverse_ (testPlugin . toText) =<< embed (lookupEnv "RIBOSOME_ROOT")
