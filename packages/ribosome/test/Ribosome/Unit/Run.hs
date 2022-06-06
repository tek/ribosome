module Ribosome.Unit.Run where

import Data.MessagePack (Object)
import Polysemy.Test (UnitTest)

import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Effect.NvimPlugin (NvimPlugin)
import Ribosome.Embed (HandlerDeps, interpretPluginEmbed, testPluginEmbed)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import Ribosome.Host.Interpret (type (|>))
import qualified Ribosome.Host.Test.Run as Host
import Ribosome.Host.Test.Run (TestStack)
import Ribosome.IOStack (TestEffects)
import Ribosome.Interpreter.NvimPlugin (interpretNvimPlugin)

type HandlerTestStack =
  HandlerDeps ++ Reader PluginName : TestStack

type PluginTestStack =
  TestEffects ++ NvimPlugin : HandlerTestStack

runTest ::
  Sem HandlerTestStack () ->
  UnitTest
runTest =
  Host.runTest .
  runReader "test" .
  interpretPluginEmbed

testHandlers ::
  Members HandlerTestStack r =>
  [RpcHandler r] ->
  Map MappingIdent (Handler r ()) ->
  Map WatchedVariable (Object -> Handler r ()) ->
  InterpretersFor (TestEffects |> NvimPlugin) r
testHandlers handlers maps vars =
  interpretNvimPlugin handlers maps vars .
  testPluginEmbed

runTestHandlers ::
  [RpcHandler HandlerTestStack] ->
  Map MappingIdent (Handler HandlerTestStack ()) ->
  Map WatchedVariable (Object -> Handler HandlerTestStack ()) ->
  Sem PluginTestStack () ->
  UnitTest
runTestHandlers handlers maps vars =
  runTest .
  testHandlers handlers maps vars

runTestRibosome ::
  Sem PluginTestStack () ->
  UnitTest
runTestRibosome =
  runTestHandlers mempty mempty mempty
