module Ribosome.Test.Run where

import Control.Lens ((^.))
import Data.Generics.Labels ()
import Data.MessagePack (Object)
import Polysemy.Test (UnitTest)

import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.PluginConfig (PluginConfig (PluginConfig))
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Effect.NvimPlugin (NvimPlugin')
import Ribosome.Embed (HandlerDeps, TestEffects, interpretPluginEmbed, testPluginEmbed)
import Ribosome.Host.Data.BootError (BootError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import Ribosome.Host.IOStack (BasicStack)
import qualified Ribosome.Host.Test.Data.TestConfig as Host
import qualified Ribosome.Host.Test.Run as Host
import Ribosome.Host.Test.Run (TestConfStack, TestStack, runUnitTest)
import Ribosome.Interpreter.NvimPlugin (pluginHandlers, rpcHandlers)
import Ribosome.Test.Data.TestConfig (TestConfig (TestConfig))

type PluginTestStack =
  HandlerDeps ++ TestStack

type StackWith r =
  TestEffects ++ NvimPlugin' ++ r ++ PluginTestStack

type Stack =
  StackWith '[]

runTestLogConf ::
  Members [Error BootError, Resource, Race, Async, Embed IO] r =>
  TestConfig ->
  InterpretersFor TestConfStack r
runTestLogConf (TestConfig freezeTime (PluginConfig _ conf)) =
  Host.runTestLogConf (Host.TestConfig freezeTime conf)

runPluginEmbedTest ::
  TestConfig ->
  Sem PluginTestStack () ->
  UnitTest
runPluginEmbedTest conf =
  runUnitTest .
  runTestLogConf conf .
  interpretPluginEmbed (conf ^. #plugin . #name)

runTest ::
  Sem PluginTestStack () ->
  UnitTest
runTest =
  runPluginEmbedTest def

testPluginHandlers ::
  Members BasicStack r =>
  Members HandlerDeps r =>
  [RpcHandler r] ->
  Map MappingIdent (Handler r ()) ->
  Map WatchedVariable (Object -> Handler r ()) ->
  InterpretersFor (TestEffects ++ NvimPlugin') r
testPluginHandlers handlers maps vars =
  pluginHandlers handlers maps vars .
  testPluginEmbed "test"

testPluginConf ::
  ∀ r .
  TestConfig ->
  Members PluginTestStack (r ++ PluginTestStack) =>
  InterpretersFor (NvimPlugin' ++ r) PluginTestStack ->
  Sem (StackWith r) () ->
  UnitTest
testPluginConf conf handlers =
  runPluginEmbedTest conf .
  handlers .
  testPluginEmbed (conf ^. #plugin . #name)

testPlugin ::
  ∀ r .
  Members PluginTestStack (r ++ PluginTestStack) =>
  InterpretersFor (NvimPlugin' ++ r) PluginTestStack ->
  Sem (StackWith r) () ->
  UnitTest
testPlugin =
  testPluginConf @r def

testPlugin_ ::
  InterpretersFor NvimPlugin' PluginTestStack ->
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
