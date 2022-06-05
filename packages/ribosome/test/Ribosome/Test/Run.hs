module Ribosome.Test.Run where

import Data.MessagePack (Object)
import Polysemy.Test (UnitTest)

import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Embed (HandlerDeps, PluginHandler, TestEffects, embedNvimPlugin)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Handlers (Handlers)
import Ribosome.Host.Test.Run (TestStack, runTest)

type PluginTestStack =
  Handlers !! HandlerError : HandlerDeps ++ TestStack

embedPluginTest ::
  Map MappingIdent (PluginHandler TestStack) ->
  Map WatchedVariable (Object -> PluginHandler TestStack) ->
  [RpcHandler (HandlerDeps ++ TestStack)] ->
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
