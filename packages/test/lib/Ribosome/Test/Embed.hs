module Ribosome.Test.Embed where

import Data.Generics.Labels ()
import Log (Severity (Trace))
import Polysemy.Test (TestError, UnitTest)

import Ribosome.Data.PluginConfig (PluginConfig (PluginConfig))
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Embed (HandlerEffects, interpretPluginEmbed, withPluginEmbed)
import Ribosome.Host.Data.BootError (BootError)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.HostConfig (setStderr)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Handlers (Handlers)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Error (resumeBootError)
import Ribosome.Host.Interpret (HigherOrder, type (|>))
import Ribosome.Host.Interpreter.Handlers (interpretHandlers)
import qualified Ribosome.Host.Test.Data.TestConfig as Host
import qualified Ribosome.Host.Test.Run as Host
import Ribosome.Host.Test.Run (TestConfStack, TestStack, runUnitTest)
import Ribosome.IOStack (BasicPluginStack)
import Ribosome.Test.Data.TestConfig (TestConfig (TestConfig))
import Ribosome.Test.Error (testError, testHandler)
import Ribosome.Test.Log (testLogLevel)

type TestEffects =
  [
    Stop HandlerError,
    Stop RpcError,
    Scratch,
    Settings,
    Rpc
  ]

type EmbedEffects =
  TestEffects |> Handlers !! HandlerError

type EmbedHandlerStack =
  HandlerEffects ++ Reader PluginName : TestStack

type EmbedStackWith r =
  EmbedEffects ++ r ++ EmbedHandlerStack

type EmbedStack =
  EmbedStackWith '[]

runTestLogConf ::
  Members [Error BootError, Resource, Race, Async, Embed IO] r =>
  TestConfig ->
  InterpretersFor (Reader PluginName : TestConfStack) r
runTestLogConf (TestConfig freezeTime (PluginConfig name conf)) =
  Host.runTestLogConf (Host.TestConfig freezeTime conf) .
  runReader name

runTestConf ::
  HasCallStack =>
  TestConfig ->
  Sem (Reader PluginName : TestStack) () ->
  UnitTest
runTestConf conf =
  runUnitTest .
  runTestLogConf conf

runEmbedTest ::
  HasCallStack =>
  TestConfig ->
  Sem EmbedHandlerStack () ->
  UnitTest
runEmbedTest conf =
  runTestConf conf .
  interpretPluginEmbed

runTest ::
  HasCallStack =>
  Sem EmbedHandlerStack () ->
  UnitTest
runTest =
  runEmbedTest def

testPluginEmbed ::
  Members HandlerEffects r =>
  Members BasicPluginStack r =>
  Member (Handlers !! HandlerError) r =>
  Member (Error TestError) r =>
  InterpretersFor TestEffects r
testPluginEmbed =
  withPluginEmbed .
  resumeBootError @Rpc .
  resumeBootError @Settings .
  resumeBootError @Scratch .
  testError .
  testHandler .
  insertAt @4

testPluginConf ::
  ∀ r .
  HasCallStack =>
  HigherOrder r EmbedHandlerStack =>
  TestConfig ->
  InterpretersFor r EmbedHandlerStack ->
  [RpcHandler (r ++ EmbedHandlerStack)] ->
  Sem (EmbedStackWith r) () ->
  UnitTest
testPluginConf conf effs handlers =
  runEmbedTest conf .
  effs .
  interpretHandlers handlers .
  testPluginEmbed

testPlugin ::
  ∀ r .
  HasCallStack =>
  HigherOrder r EmbedHandlerStack =>
  InterpretersFor r EmbedHandlerStack ->
  [RpcHandler (r ++ EmbedHandlerStack)] ->
  Sem (EmbedStackWith r) () ->
  UnitTest
testPlugin =
  testPluginConf @r def

testPlugin_ ::
  HasCallStack =>
  [RpcHandler EmbedHandlerStack] ->
  Sem EmbedStack () ->
  UnitTest
testPlugin_ =
  testPlugin @'[] id

testEmbedConf ::
  ∀ r .
  HasCallStack =>
  HigherOrder r EmbedHandlerStack =>
  TestConfig ->
  InterpretersFor r EmbedHandlerStack ->
  Sem (EmbedStackWith r) () ->
  UnitTest
testEmbedConf conf effs =
  testPluginConf @r conf effs mempty

testEmbed ::
  ∀ r .
  HasCallStack =>
  HigherOrder r EmbedHandlerStack =>
  InterpretersFor r EmbedHandlerStack ->
  Sem (EmbedStackWith r) () ->
  UnitTest
testEmbed =
  testEmbedConf @r def

testEmbedLevel ::
  ∀ r .
  HasCallStack =>
  HigherOrder r EmbedHandlerStack =>
  Severity ->
  InterpretersFor r EmbedHandlerStack ->
  Sem (EmbedStackWith r) () ->
  UnitTest
testEmbedLevel level =
  testEmbedConf @r (def & #plugin . #host %~ setStderr level)

testEmbedTrace ::
  ∀ r .
  HasCallStack =>
  HigherOrder r EmbedHandlerStack =>
  InterpretersFor r EmbedHandlerStack ->
  Sem (EmbedStackWith r) () ->
  UnitTest
testEmbedTrace effs =
  testLogLevel Trace \ conf -> testEmbedConf @r conf effs

testEmbed_ ::
  HasCallStack =>
  Sem EmbedStack () ->
  UnitTest
testEmbed_ =
  testPlugin_ mempty

testEmbedLevel_ ::
  HasCallStack =>
  Severity ->
  Sem EmbedStack () ->
  UnitTest
testEmbedLevel_ level =
  testEmbedConf @'[] (def & #plugin . #host %~ setStderr level) id

testEmbedTrace_ ::
  HasCallStack =>
  Sem EmbedStack () ->
  UnitTest
testEmbedTrace_ =
  testEmbedLevel_ Trace
