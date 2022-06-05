module Ribosome.Unit.Run where

import Data.MessagePack (Object)
import Polysemy.Test (UnitTest)

import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Effect.NvimPlugin (NvimPlugin')
import Ribosome.Embed (HandlerDeps, TestEffects, interpretPluginEmbed, testPluginEmbed)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import qualified Ribosome.Host.Test.Run as Host
import Ribosome.Host.Test.Run (TestStack)
import Ribosome.Interpreter.NvimPlugin (pluginHandlers)

type PluginTestStack =
  HandlerDeps ++ TestStack

runTest ::
  Sem PluginTestStack () ->
  UnitTest
runTest =
  Host.runTest .
  interpretPluginEmbed "test"

testHandlers ::
  Members PluginTestStack r =>
  [RpcHandler r] ->
  Map MappingIdent (Handler r ()) ->
  Map WatchedVariable (Object -> Handler r ()) ->
  InterpretersFor (TestEffects ++ NvimPlugin') r
testHandlers handlers maps vars =
  pluginHandlers handlers maps vars .
  testPluginEmbed "test"

runTestHandlers ::
  [RpcHandler PluginTestStack] ->
  Map MappingIdent (Handler PluginTestStack ()) ->
  Map WatchedVariable (Object -> Handler PluginTestStack ()) ->
  Sem (TestEffects ++ NvimPlugin' ++ PluginTestStack) () ->
  UnitTest
runTestHandlers handlers maps vars =
  runTest .
  pluginHandlers handlers maps vars .
  testPluginEmbed "test"

runTestRibosome ::
  Sem (TestEffects ++ NvimPlugin' ++ PluginTestStack) () ->
  UnitTest
runTestRibosome =
  runTestHandlers mempty mempty mempty
