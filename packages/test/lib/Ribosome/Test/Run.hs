module Ribosome.Test.Run where

import Data.MessagePack (Object)
import Polysemy.Test (UnitTest)

import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Embed (PluginHandler, PluginStack, TestEffects, embedNvimPluginConf)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Test.Data.TestConfig (TestConfig (TestConfig))
import Ribosome.Host.Test.Run (TestStack, runTestConf)

type PluginTestStack =
  PluginStack ++ TestStack

embedPluginTestConf ::
  TestConfig ->
  Map MappingIdent (PluginHandler TestStack) ->
  Map WatchedVariable (Object -> PluginHandler TestStack) ->
  [RpcHandler PluginTestStack] ->
  Sem (TestEffects ++ PluginTestStack) () ->
  UnitTest
embedPluginTestConf (TestConfig freeze conf) maps vars handlers =
  runTestConf freeze .
  embedNvimPluginConf conf "test" maps vars handlers

embedPluginTest ::
  Map MappingIdent (PluginHandler TestStack) ->
  Map WatchedVariable (Object -> PluginHandler TestStack) ->
  [RpcHandler PluginTestStack] ->
  Sem (TestEffects ++ PluginTestStack) () ->
  UnitTest
embedPluginTest =
  embedPluginTestConf def

embedPluginTestConf_ ::
  TestConfig ->
  Sem (TestEffects ++ PluginTestStack) () ->
  UnitTest
embedPluginTestConf_ (TestConfig freeze conf) =
  runTestConf freeze .
  embedNvimPluginConf conf "test" mempty mempty mempty

embedPluginTest_ ::
  Sem (TestEffects ++ PluginTestStack) () ->
  UnitTest
embedPluginTest_ =
  embedPluginTest mempty mempty mempty
