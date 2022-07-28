module Ribosome.Unit.Run where

import Data.MessagePack (Object)
import Log (Severity (Trace))
import Polysemy.Test (TestError (TestError), UnitTest)

import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Embed (HandlerEffects, embedPlugin, interpretPluginEmbed)
import Ribosome.Host.Data.HostConfig (setStderr)
import Ribosome.Host.Data.Report (Report, reportMessages)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import Ribosome.Host.Effect.Handlers (Handlers)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Error (resumeBootError)
import Ribosome.Host.Interpret (type (|>))
import Ribosome.Host.Interpreter.Handlers (interpretHandlers)
import Ribosome.Host.Test.Data.TestConfig (host)
import qualified Ribosome.Host.Test.Run as Host
import Ribosome.Host.Test.Run (TestStack)
import Ribosome.Interpreter.VariableWatcher (watchVariables)

type HandlerTestStack =
  HandlerEffects ++ Reader PluginName : TestStack

type EmbedEffects =
  [
    Stop Report,
    Scratch,
    Settings,
    Rpc
  ] |> Handlers !! Report

type PluginTestStack =
  EmbedEffects ++ HandlerTestStack

runTest ::
  HasCallStack =>
  Sem HandlerTestStack () ->
  UnitTest
runTest =
  Host.runTest .
  runReader "test" .
  interpretPluginEmbed

runTestTrace ::
  HasCallStack =>
  Sem HandlerTestStack () ->
  UnitTest
runTestTrace =
  Host.runTestConf def { host = setStderr Trace def } .
  runReader "test" .
  interpretPluginEmbed

testHandlers ::
  Members HandlerTestStack r =>
  [RpcHandler r] ->
  Map WatchedVariable (Object -> Handler r ()) ->
  InterpretersFor EmbedEffects r
testHandlers handlers vars =
  watchVariables vars .
  interpretHandlers handlers .
  embedPlugin .
  resumeBootError @Rpc .
  resumeBootError @Settings .
  resumeBootError @Scratch .
  stopToErrorWith (TestError . reportMessages) .
  insertAt @4

runTestHandlers ::
  HasCallStack =>
  [RpcHandler HandlerTestStack] ->
  Map WatchedVariable (Object -> Handler HandlerTestStack ()) ->
  Sem PluginTestStack () ->
  UnitTest
runTestHandlers handlers vars =
  runTest .
  testHandlers handlers vars

runTestRibosome ::
  HasCallStack =>
  Sem PluginTestStack () ->
  UnitTest
runTestRibosome =
  runTestHandlers mempty mempty
