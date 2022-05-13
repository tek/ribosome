module Ribosome.Host.Test.PluginTest where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Exon (exon)
import Hedgehog.Internal.Property (failWith)
import Path (Abs, Dir, Path, reldir, relfile, toFilePath, (</>))
import Path.IO (copyDirRecur)
import qualified Polysemy.Conc as Conc
import Polysemy.Conc (interpretRace)
import Polysemy.Log (Severity (Warn), interpretLogStdoutLevelConc)
import Polysemy.Process.Data.ProcessError (ProcessError)
import qualified Polysemy.Test as Test
import Polysemy.Test (Hedgehog, Test, TestError (TestError), UnitTest, assertEq, liftH, runTestAuto)
import qualified Polysemy.Time as Time
import Polysemy.Time (MilliSeconds (MilliSeconds), Minutes (Minutes), interpretTimeGhc)
import System.Environment (lookupEnv)
import System.Process.Typed (ProcessConfig, proc)

import Ribosome.Host.Api.Effect (nvimCallFunction)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (basicCliArgs, interpretRpcMsgpackProcessNvimEmbed)
import Ribosome.Host.Interpreter.RequestHandler (withRequestHandler)
import Ribosome.Host.Interpreter.Responses (interpretResponses)

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
  interpretTimeGhc $
  interpretLogStdoutLevelConc (Just Warn) $
  mapError TestError $
  mapError (show @_ @ProcessError) do
    source <- Test.fixturePath [reldir|plugin|]
    target <- Test.tempDir [reldir|plugin|]
    embed (copyDirRecur source target)
    let flake = toFilePath (target </> [relfile|flake.nix|])
    old <- embed (Text.readFile flake)
    embed (Text.writeFile flake (Text.replace "RIBOSOME" (toText riboRoot) old))
    interpretResponses $
      interpretRpcMsgpackProcessNvimEmbed (conf target) $
      withRequestHandler mempty do
        resumeHoistError @RpcError @Rpc (show @Text) do
          Conc.timeout_ (liftH (failWith Nothing "RPC function did not appear")) (Minutes 2) do
            Time.while (MilliSeconds 50) (resumeAs @RpcError True (False <$ nvimCallFunction @Int "Test" []))
          assertEq (5 :: Int) =<< nvimCallFunction "Test" []

test_plugin :: UnitTest
test_plugin =
  runTestAuto do
    traverse_ (testPlugin . toText) =<< embed (lookupEnv "RIBOSOME_ROOT")
