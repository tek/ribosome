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

import Ribosome.Effect.NvimPlugin (NvimPlugin)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.HandlerError (mapHandlerError)
import Ribosome.Host.Data.NvimSocket (NvimSocket (NvimSocket))
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Error (resumeBootError)
import Ribosome.IOStack (BasicPluginStack)
import Ribosome.Interpreter.NvimPlugin (rpcHandlers)
import Ribosome.Path (pathText)
import Ribosome.Socket (SocketHandlerEffects, interpretPluginSocket, withPluginSocket)
import Ribosome.Test.Data.TestConfig (TmuxTestConfig)
import Ribosome.Test.Embed (EmbedEffects, TestEffects)
import Ribosome.Test.Error (testHandler)
import Ribosome.Test.TmuxCommon (TmuxStack, runTmuxNvim)
import Ribosome.Test.Wait (assertWait)

type TmuxHandlerStack =
  SocketHandlerEffects ++ Reader NvimSocket : TmuxStack

type SocketTmuxWith r =
  EmbedEffects ++ r ++ TmuxHandlerStack

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
  Members [NvimPlugin, Error TestError] r =>
  InterpretersFor TestEffects r
testPluginSocket =
  withPluginSocket .
  resumeBootError @Rpc .
  resumeBootError @Settings .
  resumeBootError @Scratch .
  testHandler .
  mapHandlerError .
  insertAt @4

testPluginSocketTmuxConf ::
  ∀ r .
  HasCallStack =>
  Members TmuxHandlerStack (r ++ TmuxHandlerStack) =>
  TmuxTestConfig ->
  InterpretersFor (NvimPlugin : r) TmuxHandlerStack ->
  Sem (SocketTmuxWith r) () ->
  UnitTest
testPluginSocketTmuxConf conf handlers =
  runSocketTmuxTestConf conf .
  handlers .
  testPluginSocket

testPluginSocketTmux ::
  ∀ r .
  HasCallStack =>
  Members TmuxHandlerStack (r ++ TmuxHandlerStack) =>
  InterpretersFor (NvimPlugin : r) TmuxHandlerStack ->
  Sem (SocketTmuxWith r) () ->
  UnitTest
testPluginSocketTmux =
  testPluginSocketTmuxConf @r def

testPluginSocketTmux_ ::
  HasCallStack =>
  InterpreterFor NvimPlugin TmuxHandlerStack ->
  Sem SocketTmux () ->
  UnitTest
testPluginSocketTmux_ =
  testPluginSocketTmux @'[]

testHandlersSocketTmux ::
  HasCallStack =>
  [RpcHandler TmuxHandlerStack] ->
  Sem SocketTmux () ->
  UnitTest
testHandlersSocketTmux handlers =
  testPluginSocketTmux @'[] (rpcHandlers handlers)

testSocketTmux ::
  HasCallStack =>
  Sem SocketTmux () ->
  UnitTest
testSocketTmux =
  testHandlersSocketTmux mempty
