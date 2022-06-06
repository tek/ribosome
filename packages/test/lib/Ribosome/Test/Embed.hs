module Ribosome.Test.Embed where

import Data.Generics.Labels ()
import Data.MessagePack (Object)
import Polysemy.Test (UnitTest)

import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.PluginConfig (PluginConfig (PluginConfig))
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Effect.NvimPlugin (NvimPlugin)
import Ribosome.Embed (HandlerDeps, interpretPluginEmbed, testPluginEmbed)
import Ribosome.Host.Data.BootError (BootError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import Ribosome.Host.Interpret (type (|>))
import qualified Ribosome.Host.Test.Data.TestConfig as Host
import qualified Ribosome.Host.Test.Run as Host
import Ribosome.Host.Test.Run (TestConfStack, TestStack, runUnitTest)
import Ribosome.IOStack (BasicPluginStack, TestEffects)
import Ribosome.Interpreter.NvimPlugin (interpretNvimPlugin, rpcHandlers)
import Ribosome.Test.Data.TestConfig (TestConfig (TestConfig))

type PluginTestStack =
  HandlerDeps ++ Reader PluginName : TestStack

type StackWith r =
  TestEffects ++ NvimPlugin : r ++ PluginTestStack

type Stack =
  StackWith '[]

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

runPluginEmbedTest ::
  TestConfig ->
  Sem PluginTestStack () ->
  UnitTest
runPluginEmbedTest conf =
  runTestConf conf .
  interpretPluginEmbed

runTest ::
  Sem PluginTestStack () ->
  UnitTest
runTest =
  runPluginEmbedTest def

testPluginHandlers ::
  Members BasicPluginStack r =>
  Members HandlerDeps r =>
  [RpcHandler r] ->
  Map MappingIdent (Handler r ()) ->
  Map WatchedVariable (Object -> Handler r ()) ->
  InterpretersFor (TestEffects |> NvimPlugin) r
testPluginHandlers handlers maps vars =
  interpretNvimPlugin handlers maps vars .
  testPluginEmbed

testPluginConf ::
  ∀ r .
  TestConfig ->
  Members PluginTestStack (r ++ PluginTestStack) =>
  InterpretersFor (NvimPlugin : r) PluginTestStack ->
  Sem (StackWith r) () ->
  UnitTest
testPluginConf conf handlers =
  runPluginEmbedTest conf .
  handlers .
  testPluginEmbed

testPlugin ::
  ∀ r .
  Members PluginTestStack (r ++ PluginTestStack) =>
  InterpretersFor (NvimPlugin : r) PluginTestStack ->
  Sem (StackWith r) () ->
  UnitTest
testPlugin =
  testPluginConf @r def

testPlugin_ ::
  InterpreterFor NvimPlugin PluginTestStack ->
  Sem Stack () ->
  UnitTest
testPlugin_ =
  testPlugin @'[]

testHandlers ::
  [RpcHandler PluginTestStack] ->
  Sem Stack () ->
  UnitTest
testHandlers handlers =
  testPlugin @'[] (rpcHandlers handlers)

testRibosome ::
  Sem Stack () ->
  UnitTest
testRibosome =
  testHandlers mempty
