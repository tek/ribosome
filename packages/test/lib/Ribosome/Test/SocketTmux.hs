module Ribosome.Test.SocketTmux where

import Chiasma.Command.Pane (sendKeys)
import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.SendKeysParams (Key (Lit))
import Chiasma.Effect.Codec (NativeCommandCodecE)
import Chiasma.Effect.TmuxApi (Tmux)
import Chiasma.Effect.TmuxClient (NativeTmux)
import Chiasma.Tmux (withTmux)
import Exon (exon)
import Hedgehog.Internal.Property (Failure)
import Path (Abs, File, Path, reldir, relfile, (</>))
import Path.IO (doesPathExist)
import Polysemy.Chronos (ChronosTime)
import qualified Polysemy.Test as Test
import Polysemy.Test (Hedgehog, Test, TestError, UnitTest, assert)

import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.NvimSocket (NvimSocket (NvimSocket))
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Error (resumeBootError)
import Ribosome.Host.Interpreter.Handlers (withHandlers)
import Ribosome.IOStack (BasicPluginStack)
import Ribosome.Path (pathText)
import Ribosome.Socket (SocketHandlerEffects, interpretPluginSocket, socketPlugin)
import Ribosome.Test.Data.TestConfig (TmuxTestConfig)
import Ribosome.Test.Embed (TestEffects)
import Ribosome.Test.Error (testError, testHandler)
import Ribosome.Test.TmuxCommon (TmuxStack, runTmuxNvim)
import Ribosome.Test.Wait (assertWait)

type TmuxHandlerStack =
  SocketHandlerEffects ++ Reader NvimSocket : TmuxStack

type SocketTmuxWith r =
  TestEffects ++ r ++ TmuxHandlerStack

type SocketTmux =
  SocketTmuxWith '[]

nvimCmdline :: Path Abs File -> Text
nvimCmdline socket =
  [exon|nvim --listen #{pathText socket} -n -u NONE -i NONE --clean|]

withSocketTmuxNvim ::
  Members [Test, Hedgehog IO, ChronosTime, Error Failure, Race, Embed IO] r =>
  Members [NativeTmux, NativeCommandCodecE, Stop CodecError] r =>
  InterpreterFor (Reader NvimSocket) r
withSocketTmuxNvim sem = do
  dir <- Test.tempDir [reldir|tmux-test|]
  let socket = dir </> [relfile|nvim-socket|]
  withTmux do
    restop @CodecError @Tmux $ sendKeys 0 [Lit (nvimCmdline socket)]
  assertWait (pure socket) (assert <=< doesPathExist)
  runReader (NvimSocket socket) sem

runSocketTmuxTestConf ::
  HasCallStack =>
  TmuxTestConfig ->
  Sem TmuxHandlerStack () ->
  UnitTest
runSocketTmuxTestConf conf =
  runTmuxNvim conf .
  withSocketTmuxNvim .
  interpretPluginSocket

runSocketTmuxTest ::
  HasCallStack =>
  Sem TmuxHandlerStack () ->
  UnitTest
runSocketTmuxTest =
  runSocketTmuxTestConf def

runSocketTmuxGuiTest ::
  HasCallStack =>
  Sem TmuxHandlerStack () ->
  UnitTest
runSocketTmuxGuiTest =
  runSocketTmuxTestConf (def & #tmux . #gui .~ True)

testPluginSocket ::
  Members BasicPluginStack r =>
  Members SocketHandlerEffects r =>
  Member (Error TestError) r =>
  InterpretersFor TestEffects r
testPluginSocket =
  socketPlugin .
  resumeBootError @Rpc .
  resumeBootError @Settings .
  resumeBootError @Scratch .
  testError .
  testHandler .
  insertAt @4

testPluginSocketTmuxConf ::
  ∀ r .
  HasCallStack =>
  Members TmuxHandlerStack (r ++ TmuxHandlerStack) =>
  TmuxTestConfig ->
  InterpretersFor r TmuxHandlerStack ->
  [RpcHandler (r ++ TmuxHandlerStack)] ->
  Sem (SocketTmuxWith r) () ->
  UnitTest
testPluginSocketTmuxConf conf effs handlers =
  runSocketTmuxTestConf conf .
  effs .
  withHandlers handlers .
  testPluginSocket

testPluginSocketTmux ::
  ∀ r .
  HasCallStack =>
  Members TmuxHandlerStack (r ++ TmuxHandlerStack) =>
  InterpretersFor r TmuxHandlerStack ->
  [RpcHandler (r ++ TmuxHandlerStack)] ->
  Sem (SocketTmuxWith r) () ->
  UnitTest
testPluginSocketTmux =
  testPluginSocketTmuxConf @r def

testPluginSocketTmux_ ::
  HasCallStack =>
  [RpcHandler TmuxHandlerStack] ->
  Sem SocketTmux () ->
  UnitTest
testPluginSocketTmux_ =
  testPluginSocketTmux @'[] id

testSocketTmux ::
  HasCallStack =>
  Sem SocketTmux () ->
  UnitTest
testSocketTmux =
  testPluginSocketTmux_ mempty
