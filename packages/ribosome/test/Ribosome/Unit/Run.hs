module Ribosome.Unit.Run where

import Data.MessagePack (Object)
import Polysemy.Test (TestError (TestError), UnitTest)

import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Effect.NvimPlugin (NvimPlugin)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Embed (HandlerEffects, interpretPluginEmbed, withPluginEmbed)
import Ribosome.Host.Data.HandlerError (HandlerError, handlerErrorMessage)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Error (resumeBootError)
import qualified Ribosome.Host.Test.Run as Host
import Ribosome.Host.Test.Run (TestStack)
import Ribosome.Interpreter.NvimPlugin (interpretNvimPlugin)

type HandlerTestStack =
  HandlerEffects ++ Reader PluginName : TestStack

type EmbedEffects =
  [
    Stop HandlerError,
    Scratch,
    Settings,
    Rpc,
    NvimPlugin
  ]

type PluginTestStack =
  EmbedEffects ++ HandlerTestStack

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
  InterpretersFor EmbedEffects r
testHandlers handlers maps vars =
  interpretNvimPlugin handlers maps vars .
  withPluginEmbed .
  resumeBootError @Rpc .
  resumeBootError @Settings .
  resumeBootError @Scratch .
  stopToErrorWith (TestError . handlerErrorMessage) .
  insertAt @4

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
