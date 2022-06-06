module Ribosome.Test.Tmux where

import Chiasma.Command.Pane (sendKeys)
import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.SendKeysParams (Key (Lit))
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Effect.TmuxApi (Tmux)
import Chiasma.Effect.TmuxClient (NativeTmux)
import Chiasma.Test.Tmux (TestTmuxEffects, TmuxTestConf (ttcGui), withSystemTempDir, withTestTmux)
import Chiasma.Tmux (withTmux)
import Data.MessagePack (Object)
import Exon (exon)
import Hedgehog.Internal.Property (Failure)
import Path (Abs, File, Path, reldir, relfile, (</>))
import Path.IO (doesPathExist)
import Polysemy.Chronos (ChronosTime)
import qualified Polysemy.Test as Test
import Polysemy.Test (Hedgehog, Test, UnitTest, assert)

import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Effect.NvimPlugin (NvimPlugin)
import Ribosome.Host.Data.BootError (BootError (BootError))
import Ribosome.Host.Data.NvimSocket (NvimSocket (NvimSocket))
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import Ribosome.Host.Interpret (type (|>))
import Ribosome.Host.Test.Run (TestStack)
import Ribosome.IOStack (BasicPluginStack, TestEffects)
import Ribosome.Interpreter.NvimPlugin (interpretNvimPlugin, rpcHandlers)
import Ribosome.Path (pathText)
import Ribosome.Socket (HandlerDeps, interpretPluginSocket, testPluginSocket)
import Ribosome.Test.Data.TestConfig (TestConfig)
import Ribosome.Test.Embed (runTestConf)
import Ribosome.Test.Wait (assertWait)

type TmuxErrors =
  [
    Stop CodecError,
    Error CodecError,
    Stop RenderError,
    Error RenderError,
    Stop TmuxError,
    Error TmuxError,
    Error Text
  ]

type TmuxStack =
  TestTmuxEffects ++ TmuxErrors

type TmuxTestStack =
  Reader NvimSocket : Tmux : TmuxStack ++ Reader PluginName : TestStack

type HandlerStack =
  HandlerDeps ++ TmuxTestStack

type StackWith r =
  TestEffects ++ NvimPlugin : r ++ HandlerStack

type Stack =
  StackWith '[]

tmuxConf :: TmuxTestConf
tmuxConf =
  def { ttcGui = True }

interpretTmuxErrors ::
  Member (Error BootError) r =>
  InterpretersFor TmuxErrors r
interpretTmuxErrors =
  mapError BootError .
  mapError @TmuxError (BootError . show) .
  stopToError .
  mapError @RenderError (BootError . show) .
  stopToError .
  mapError @CodecError (BootError . show) .
  stopToError

withTmuxTest ::
  Members TestStack r =>
  TmuxTestConf ->
  InterpretersFor TmuxStack r
withTmuxTest conf =
  interpretTmuxErrors .
  withSystemTempDir .
  withTestTmux conf

nvimCmdline :: Path Abs File -> Text
nvimCmdline socket =
  [exon|nvim --listen #{pathText socket} -n -u NONE -i NONE --clean|]

withTmuxNvim ::
  Members [Tmux, Test, Hedgehog IO, ChronosTime, Error Failure, Race, Embed IO] r =>
  InterpreterFor (Reader NvimSocket) r
withTmuxNvim sem = do
  dir <- Test.tempDir [reldir|tmux-test|]
  let socket = dir </> [relfile|nvim-socket|]
  sendKeys 0 [Lit (nvimCmdline socket)]
  assertWait (pure socket) (assert <=< doesPathExist)
  runReader (NvimSocket socket) sem

runTmuxNvim ::
  TestConfig ->
  Sem TmuxTestStack () ->
  UnitTest
runTmuxNvim conf =
  runTestConf conf .
  withTmuxTest tmuxConf .
  restop @TmuxError @NativeTmux .
  withTmux .
  restop @CodecError @Tmux .
  raiseUnder2 .
  withTmuxNvim

runPluginTmuxTest ::
  TestConfig ->
  Sem HandlerStack () ->
  UnitTest
runPluginTmuxTest conf =
  runTmuxNvim conf .
  interpretPluginSocket

runTest ::
  Sem HandlerStack () ->
  UnitTest
runTest =
  runPluginTmuxTest def

testPluginTmuxHandlers ::
  Members BasicPluginStack r =>
  Members HandlerDeps r =>
  [RpcHandler r] ->
  Map MappingIdent (Handler r ()) ->
  Map WatchedVariable (Object -> Handler r ()) ->
  InterpretersFor (TestEffects |> NvimPlugin) r
testPluginTmuxHandlers handlers maps vars =
  interpretNvimPlugin handlers maps vars .
  testPluginSocket

testPluginTmuxConf ::
  ∀ r .
  TestConfig ->
  Members HandlerStack (r ++ HandlerStack) =>
  InterpretersFor (NvimPlugin : r) HandlerStack ->
  Sem (StackWith r) () ->
  UnitTest
testPluginTmuxConf conf handlers =
  runPluginTmuxTest conf .
  handlers .
  testPluginSocket

testPluginTmux ::
  ∀ r .
  Members HandlerStack (r ++ HandlerStack) =>
  InterpretersFor (NvimPlugin : r) HandlerStack ->
  Sem (StackWith r) () ->
  UnitTest
testPluginTmux =
  testPluginTmuxConf @r def

testPluginTmux_ ::
  InterpreterFor NvimPlugin HandlerStack ->
  Sem Stack () ->
  UnitTest
testPluginTmux_ =
  testPluginTmux @'[]

testHandlersTmux ::
  [RpcHandler HandlerStack] ->
  Sem Stack () ->
  UnitTest
testHandlersTmux handlers =
  testPluginTmux @'[] (rpcHandlers handlers)

testRibosomeTmux ::
  Sem Stack () ->
  UnitTest
testRibosomeTmux =
  testHandlersTmux mempty
