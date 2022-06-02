module Ribosome.Test.Run where

import Data.MessagePack (Object)
import Polysemy.Test (UnitTest)

import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.PluginConfig (PluginConfig (PluginConfig))
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Embed (PluginHandler, PluginStack, TestEffects, embedNvimPluginConf, interpretPlugin, testPlugin)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Test.Run (TestStack, runTestConf)
import Ribosome.Test.Data.TestConfig (TestConfig (TestConfig))

type PluginTestStack =
  PluginStack ++ TestStack

embedPluginTestConf ::
  ∀ r .
  Members PluginTestStack (r ++ PluginTestStack) =>
  TestConfig ->
  Map MappingIdent (PluginHandler TestStack) ->
  Map WatchedVariable (Object -> PluginHandler TestStack) ->
  [RpcHandler (r ++ PluginTestStack)] ->
  InterpretersFor r PluginTestStack ->
  Sem (TestEffects ++ r ++ PluginTestStack) () ->
  UnitTest
embedPluginTestConf (TestConfig freeze (PluginConfig name conf)) maps vars handlers effs =
  runTestConf freeze .
  interpretPlugin conf name maps vars .
  effs .
  testPlugin name handlers

embedPluginTest ::
  Map MappingIdent (PluginHandler TestStack) ->
  Map WatchedVariable (Object -> PluginHandler TestStack) ->
  [RpcHandler PluginTestStack] ->
  Sem (TestEffects ++ PluginTestStack) () ->
  UnitTest
embedPluginTest maps vars handlers =
  embedPluginTestConf @'[] def maps vars handlers id

embedPluginTestWith ::
  ∀ r .
  Members PluginTestStack (r ++ PluginTestStack) =>
  [RpcHandler (r ++ PluginTestStack)] ->
  InterpretersFor r PluginTestStack ->
  Sem (TestEffects ++ r ++ PluginTestStack) () ->
  UnitTest
embedPluginTestWith =
  embedPluginTestConf @r def mempty mempty

embedPluginTestWith_ ::
  ∀ r .
  Members PluginTestStack (r ++ PluginTestStack) =>
  InterpretersFor r PluginTestStack ->
  Sem (TestEffects ++ r ++ PluginTestStack) () ->
  UnitTest
embedPluginTestWith_ =
  embedPluginTestWith @r mempty

embedPluginTestConf_ ::
  TestConfig ->
  Sem (TestEffects ++ PluginTestStack) () ->
  UnitTest
embedPluginTestConf_ (TestConfig freeze (PluginConfig name conf)) =
  runTestConf freeze .
  embedNvimPluginConf conf name mempty mempty mempty

embedPluginTest_ ::
  Sem (TestEffects ++ PluginTestStack) () ->
  UnitTest
embedPluginTest_ =
  embedPluginTest mempty mempty mempty
