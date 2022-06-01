module Ribosome.Test.Run where

import Data.MessagePack (Object)
import Polysemy.Test (UnitTest)

import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Embed (PluginHandler, PluginStack, TestEffects, embedNvimPlugin)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Test.Run (TestStack, runTest)

type PluginTestStack =
  PluginStack ++ TestStack

embedPluginTest ::
  Map MappingIdent (PluginHandler TestStack) ->
  Map WatchedVariable (Object -> PluginHandler TestStack) ->
  [RpcHandler PluginTestStack] ->
  Sem (TestEffects ++ PluginTestStack) () ->
  UnitTest
embedPluginTest maps vars handlers =
  runTest .
  embedNvimPlugin "test" maps vars handlers

embedPluginTest_ ::
  Sem (TestEffects ++ PluginTestStack) () ->
  UnitTest
embedPluginTest_ =
  embedPluginTest mempty mempty mempty
