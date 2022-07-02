module Ribosome.Test.Embed where

import Data.Generics.Labels ()
import Data.MessagePack (Object)
import Polysemy.Test (TestError, UnitTest)

import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.PluginConfig (PluginConfig (PluginConfig))
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Effect.NvimPlugin (NvimPlugin)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Embed (HandlerEffects, interpretPluginEmbed, withPluginEmbed)
import Ribosome.Host.Data.BootError (BootError)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Error (resumeBootError)
import Ribosome.Host.Interpret (HigherOrder)
import qualified Ribosome.Host.Test.Data.TestConfig as Host
import qualified Ribosome.Host.Test.Run as Host
import Ribosome.Host.Test.Run (TestConfStack, TestStack, runUnitTest)
import Ribosome.IOStack (BasicPluginStack)
import Ribosome.Interpreter.NvimPlugin (interpretNvimPlugin, rpcHandlers)
import Ribosome.Test.Data.TestConfig (TestConfig (TestConfig))
import Ribosome.Test.Error (testHandler)

type TestEffects =
  [
    Stop HandlerError,
    Scratch,
    Settings,
    Rpc
  ]

type EmbedEffects =
  [
    Stop HandlerError,
    Scratch,
    Settings,
    Rpc,
    NvimPlugin
  ]

type EmbedHandlerStack =
  HandlerEffects ++ Reader PluginName : TestStack

type EmbedStackWith r =
  EmbedEffects ++ r ++ EmbedHandlerStack

type EmbedStack =
  EmbedStackWith '[]

runTestLogConf ::
  Members [Error BootError, Resource, Race, Async, Embed IO] r =>
  TestConfig ->
  InterpretersFor (Reader PluginName : TestConfStack) r
runTestLogConf (TestConfig freezeTime (PluginConfig name conf)) =
  Host.runTestLogConf (Host.TestConfig freezeTime conf) .
  runReader name

runTestConf ::
  HasCallStack =>
  TestConfig ->
  Sem (Reader PluginName : TestStack) () ->
  UnitTest
runTestConf conf =
  runUnitTest .
  runTestLogConf conf

runEmbedTest ::
  HasCallStack =>
  TestConfig ->
  Sem EmbedHandlerStack () ->
  UnitTest
runEmbedTest conf =
  runTestConf conf .
  interpretPluginEmbed

runTest ::
  HasCallStack =>
  Sem EmbedHandlerStack () ->
  UnitTest
runTest =
  runEmbedTest def

testPluginEmbed ::
  Members HandlerEffects r =>
  Members BasicPluginStack r =>
  Members [NvimPlugin, Error TestError] r =>
  InterpretersFor TestEffects r
testPluginEmbed =
  withPluginEmbed .
  resumeBootError @Rpc .
  resumeBootError @Settings .
  resumeBootError @Scratch .
  testHandler .
  insertAt @4

testPluginHandlers ::
  Members HandlerEffects r =>
  Members BasicPluginStack r =>
  Member (Error TestError) r =>
  [RpcHandler r] ->
  Map MappingIdent (Handler r ()) ->
  Map WatchedVariable (Object -> Handler r ()) ->
  InterpretersFor EmbedEffects r
testPluginHandlers handlers maps vars =
  interpretNvimPlugin handlers maps vars .
  testPluginEmbed

testPluginConf ::
  ∀ r .
  HasCallStack =>
  HigherOrder r EmbedHandlerStack =>
  TestConfig ->
  InterpretersFor (NvimPlugin : r) EmbedHandlerStack ->
  Sem (EmbedStackWith r) () ->
  UnitTest
testPluginConf conf handlers =
  runEmbedTest conf .
  handlers .
  testPluginEmbed

testPlugin ::
  ∀ r .
  HasCallStack =>
  HigherOrder r EmbedHandlerStack =>
  InterpretersFor (NvimPlugin : r) EmbedHandlerStack ->
  Sem (EmbedStackWith r) () ->
  UnitTest
testPlugin =
  testPluginConf @r def

testPlugin_ ::
  HasCallStack =>
  InterpreterFor NvimPlugin EmbedHandlerStack ->
  Sem EmbedStack () ->
  UnitTest
testPlugin_ =
  testPlugin @'[]

testHandlersConf ::
  ∀ r .
  HasCallStack =>
  HigherOrder r EmbedHandlerStack =>
  TestConfig ->
  InterpretersFor r EmbedHandlerStack ->
  [RpcHandler (r ++ EmbedHandlerStack)] ->
  Sem (EmbedStackWith r) () ->
  UnitTest
testHandlersConf conf effs handlers =
  testPluginConf @r conf (effs . rpcHandlers handlers)

testHandlers ::
  ∀ r .
  HasCallStack =>
  HigherOrder r EmbedHandlerStack =>
  InterpretersFor r EmbedHandlerStack ->
  [RpcHandler (r ++ EmbedHandlerStack)] ->
  Sem (EmbedStackWith r) () ->
  UnitTest
testHandlers =
  testHandlersConf @r def

testHandlers_ ::
  HasCallStack =>
  [RpcHandler EmbedHandlerStack] ->
  Sem EmbedStack () ->
  UnitTest
testHandlers_ =
  testHandlers @'[] id

testEmbedConf ::
  ∀ r .
  HasCallStack =>
  HigherOrder r EmbedHandlerStack =>
  TestConfig ->
  InterpretersFor r EmbedHandlerStack ->
  Sem (EmbedStackWith r) () ->
  UnitTest
testEmbedConf conf effs =
  testHandlersConf @r conf effs mempty

testEmbed ::
  ∀ r .
  HasCallStack =>
  HigherOrder r EmbedHandlerStack =>
  InterpretersFor r EmbedHandlerStack ->
  Sem (EmbedStackWith r) () ->
  UnitTest
testEmbed =
  testEmbedConf @r def

testEmbed_ ::
  HasCallStack =>
  Sem EmbedStack () ->
  UnitTest
testEmbed_ =
  testHandlers_ mempty
