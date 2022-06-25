module Ribosome.Test.SocketTmux where

import Chiasma.Command.Pane (sendKeys)
import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.SendKeysParams (Key (Lit))
import Chiasma.Effect.Codec (NativeCommandCodecE)
import Chiasma.Effect.TmuxApi (Tmux)
import Chiasma.Effect.TmuxClient (NativeTmux)
import Chiasma.Tmux (withTmux)
import Control.Lens ((.~))
import Exon (exon)
import Hedgehog.Internal.Property (Failure)
import Path (Abs, File, Path, reldir, relfile, (</>))
import Path.IO (doesPathExist)
import Polysemy.Chronos (ChronosTime)
import qualified Polysemy.Test as Test
import Polysemy.Test (Hedgehog, Test, UnitTest, assert)

import Ribosome.Effect.NvimPlugin (NvimPlugin)
import Ribosome.Host.Data.NvimSocket (NvimSocket (NvimSocket))
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.IOStack (TestEffects)
import Ribosome.Interpreter.NvimPlugin (rpcHandlers)
import Ribosome.Path (pathText)
import Ribosome.Socket (HandlerDeps, interpretPluginSocket, testPluginSocket)
import Ribosome.Test.Data.TestConfig (TmuxTestConfig)
import Ribosome.Test.TmuxCommon (TmuxStack, runTmuxNvim)
import Ribosome.Test.Wait (assertWait)

type HandlerStack =
  HandlerDeps ++ Reader NvimSocket : TmuxStack

type SocketTmuxWith r =
  TestEffects ++ NvimPlugin : r ++ HandlerStack

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
  Sem HandlerStack () ->
  UnitTest
runSocketTmuxTestConf conf =
  runTmuxNvim conf .
  withSocketTmuxNvim .
  interpretPluginSocket

runSocketTmuxTest ::
  HasCallStack =>
  Sem HandlerStack () ->
  UnitTest
runSocketTmuxTest =
  runSocketTmuxTestConf def

runSocketTmuxGuiTest ::
  HasCallStack =>
  Sem HandlerStack () ->
  UnitTest
runSocketTmuxGuiTest =
  runSocketTmuxTestConf (def & #tmux . #gui .~ True)

testPluginSocketTmuxConf ::
  ∀ r .
  HasCallStack =>
  Members HandlerStack (r ++ HandlerStack) =>
  TmuxTestConfig ->
  InterpretersFor (NvimPlugin : r) HandlerStack ->
  Sem (SocketTmuxWith r) () ->
  UnitTest
testPluginSocketTmuxConf conf handlers =
  runSocketTmuxTestConf conf .
  handlers .
  testPluginSocket

testPluginSocketTmux ::
  ∀ r .
  HasCallStack =>
  Members HandlerStack (r ++ HandlerStack) =>
  InterpretersFor (NvimPlugin : r) HandlerStack ->
  Sem (SocketTmuxWith r) () ->
  UnitTest
testPluginSocketTmux =
  testPluginSocketTmuxConf @r def

testPluginSocketTmux_ ::
  HasCallStack =>
  InterpreterFor NvimPlugin HandlerStack ->
  Sem SocketTmux () ->
  UnitTest
testPluginSocketTmux_ =
  testPluginSocketTmux @'[]

testHandlersSocketTmux ::
  HasCallStack =>
  [RpcHandler HandlerStack] ->
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
