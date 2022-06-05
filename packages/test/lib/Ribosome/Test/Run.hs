module Ribosome.Test.Run where

import Control.Lens ((^.))
import Data.Generics.Labels ()
import Data.MessagePack (Object)
import Polysemy.Test (UnitTest)

import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.PluginConfig (PluginConfig (PluginConfig))
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Embed (HandlerDeps, TestEffects, interpretPluginEmbed, testPluginEmbed)
import Ribosome.Host.Data.BootError (BootError)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import Ribosome.Host.Effect.Handlers (Handlers)
import Ribosome.Host.Interpreter.Handlers (interpretHandlers, interpretHandlersNull)
import qualified Ribosome.Host.Test.Data.TestConfig as Host
import qualified Ribosome.Host.Test.Run as Host
import Ribosome.Host.Test.Run (TestConfStack, TestStack, runUnitTest)
import Ribosome.Test.Data.TestConfig (TestConfig (TestConfig))

type PluginTestStack =
  HandlerDeps ++ TestStack

type PluginHandler r =
  Handler (Handlers !! HandlerError : HandlerDeps ++ r) ()

type Stack r =
  TestEffects ++ Handlers !! HandlerError : r ++ PluginTestStack

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
  interpretPluginEmbed name
  where
    name =
      conf ^. #plugin . #name

embedPluginTestConf ::
  TestConfig ->
  Map MappingIdent (PluginHandler TestStack) ->
  Map WatchedVariable (Object -> PluginHandler TestStack) ->
  [RpcHandler PluginTestStack] ->
  Sem (Stack '[]) () ->
  UnitTest
embedPluginTestConf conf maps vars handlers =
  runPluginEmbedTest conf .
  interpretHandlers handlers .
  testPluginEmbed "test" maps vars

embedPluginTest ::
  Map MappingIdent (PluginHandler TestStack) ->
  Map WatchedVariable (Object -> PluginHandler TestStack) ->
  [RpcHandler PluginTestStack] ->
  Sem (Stack '[]) () ->
  UnitTest
embedPluginTest =
  embedPluginTestConf def

embedPluginTestWith ::
  ∀ r .
  Members PluginTestStack (r ++ PluginTestStack) =>
  [RpcHandler (r ++ PluginTestStack)] ->
  InterpretersFor r PluginTestStack ->
  Sem (Stack r) () ->
  UnitTest
embedPluginTestWith handlers effs =
  runPluginEmbedTest def .
  effs .
  interpretHandlers handlers .
  testPluginEmbed "test" mempty mempty

embedPluginTestWith_ ::
  ∀ r .
  Members PluginTestStack (r ++ PluginTestStack) =>
  InterpretersFor r PluginTestStack ->
  Sem (Stack r) () ->
  UnitTest
embedPluginTestWith_ =
  embedPluginTestWith @r mempty

embedPluginTestConf_ ::
  TestConfig ->
  Sem (Stack '[]) () ->
  UnitTest
embedPluginTestConf_ conf =
  runPluginEmbedTest conf .
  interpretHandlersNull .
  testPluginEmbed "test" mempty mempty

embedPluginTest_ ::
  Sem (Stack '[]) () ->
  UnitTest
embedPluginTest_ =
  embedPluginTestConf_ def
