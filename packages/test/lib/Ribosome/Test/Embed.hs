module Ribosome.Test.Embed (
  -- * Embedded Neovim testing
  -- $intro

  testPlugin,
  testEmbed,
  testPluginEmbed,
  runEmbedTest,
  runTest,
  testPluginConf,
  testPlugin_,
  testEmbedConf,
  testEmbed_,
  testEmbedLevel,
  testEmbedLevel_,
  testEmbedTrace,
  testEmbedTrace_,
  runTestConf,
  runTestLogConf,
  EmbedStackWith,
  EmbedStack,
  EmbedHandlerStack,
  TestEffects,
) where

import Log (Severity (Trace))
import Polysemy.Test (TestError, UnitTest)

import Ribosome.Data.PluginConfig (PluginConfig (PluginConfig))
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Embed (HandlerEffects, embedPlugin, interpretPluginEmbed)
import Ribosome.Host.Data.BootError (BootError)
import Ribosome.Host.Data.HostConfig (setStderr)
import Ribosome.Host.Data.Report (Report)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Error (resumeBootError)
import Ribosome.Host.Interpret (HigherOrder)
import Ribosome.Host.Interpreter.Handlers (withHandlers)
import Ribosome.Host.Interpreter.Host (HostDeps)
import qualified Ribosome.Host.Test.Data.TestConfig as Host
import qualified Ribosome.Host.Test.Run as Host
import Ribosome.Host.Test.Run (TestConfStack, TestStack, runUnitTest)
import Ribosome.Plugin.Builtin (BuiltinHandlersDeps)
import Ribosome.Test.Data.TestConfig (TestConfig (TestConfig))
import Ribosome.Test.Error (testError, testHandler)
import Ribosome.Test.Log (testLogLevel)

-- $intro
-- The function 'testPluginEmbed' starts an embedded Neovim subprocess and a Ribosome main loop, then executes the
-- supplied 'Sem'.
--
-- This can be interpreted into a "Hedgehog" 'Hedgehog.TestT' by using the functions 'runEmbedTest' and 'runTest'.
--
-- The functions 'testPluginConf' and 'testPlugin' run a full Ribosome plugin with RPC handlers and extra effects in
-- addition to the above.
-- This can be used to test calling RPC handlers from Neovim, which usually shouldn't be necessary but may be helpful
-- for some edge cases.
--
-- The functions 'testEmbedConf' and 'testEmbed' run tests with extra effects, but no handlers.
-- This is the most advisable way to test plugins, running handlers directly as Haskell functions instead of routing
-- them through Neovim, in particular for those that don't have any parameters.

-- |The extra effects that tests are expected to use, related to errors.
--
-- The plugin effects 'Scratch', 'Settings' and 'Rpc' are allowed without 'Resume', causing tests to terminate
-- immediately if one of these effects is used and throws an error.
--
-- Additionally, the two core errors, 'LogReport' and 'RpcError' are executed directly via 'Stop'.
type TestEffects =
  [
    Stop Report,
    Stop RpcError,
    Scratch,
    Settings,
    Rpc
  ]

-- |The full test stack below test effects and extra effects.
type EmbedHandlerStack =
  HandlerEffects ++ Reader PluginName : TestStack

-- |The full test stack with additional effects.
type EmbedStackWith r =
  TestEffects ++ r ++ EmbedHandlerStack

-- |The full test stack with no additional effects.
type EmbedStack =
  EmbedStackWith '[]

-- |Interpret the basic test effects without 'IO' related effects.
runTestLogConf ::
  Members [Error BootError, Resource, Race, Async, Embed IO] r =>
  TestConfig ->
  InterpretersFor (Reader PluginName : TestConfStack) r
runTestLogConf (TestConfig freezeTime (PluginConfig name conf _)) =
  Host.runTestLogConf (Host.TestConfig freezeTime conf) .
  runReader name

-- |Run the basic test effects as a "Hedgehog" test.
runTestConf ::
  HasCallStack =>
  TestConfig ->
  Sem (Reader PluginName : TestStack) () ->
  UnitTest
runTestConf conf =
  runUnitTest .
  runTestLogConf conf

-- |Run the plugin stack and the test stack, using the supplied config.
runEmbedTest ::
  HasCallStack =>
  TestConfig ->
  Sem EmbedHandlerStack () ->
  UnitTest
runEmbedTest conf =
  runTestConf conf .
  interpretPluginEmbed

-- |Run the plugin stack and the test stack, using the default config.
runTest ::
  HasCallStack =>
  Sem EmbedHandlerStack () ->
  UnitTest
runTest =
  runEmbedTest def

-- |Run the test plugin effects, 'TestEffects', and start an embedded Neovim subprocess.
testPluginEmbed ::
  Members (HostDeps er) r =>
  Members BuiltinHandlersDeps r =>
  Members [Settings !! SettingError, Error TestError] r =>
  InterpretersFor TestEffects r
testPluginEmbed =
  embedPlugin .
  resumeBootError @Rpc .
  resumeBootError @Settings .
  resumeBootError @Scratch .
  testError .
  testHandler .
  insertAt @4

-- |Run a full plugin test, using extra effects and RPC handlers.
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
  withHandlers handlers .
  testPluginEmbed

-- |Run a full plugin test, using extra effects and RPC handlers.
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

-- |Run a plugin test with RPC handlers.
testPlugin_ ::
  HasCallStack =>
  [RpcHandler EmbedHandlerStack] ->
  Sem EmbedStack () ->
  UnitTest
testPlugin_ =
  testPlugin @'[] id

-- |Run a plugin test with extra effects but no RPC handlers.
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

-- |Run a plugin test with extra effects but no RPC handlers.
testEmbed ::
  ∀ r .
  HasCallStack =>
  HigherOrder r EmbedHandlerStack =>
  InterpretersFor r EmbedHandlerStack ->
  Sem (EmbedStackWith r) () ->
  UnitTest
testEmbed =
  testEmbedConf @r def

-- |Run a plugin test with extra effects but no RPC handlers.
--
-- Takes a log level, for which the default is to only print critical errors.
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

-- |Run a plugin test with extra effects but no RPC handlers at the 'Trace' log level for debugging RPC traffic.
testEmbedTrace ::
  ∀ r .
  HasCallStack =>
  HigherOrder r EmbedHandlerStack =>
  InterpretersFor r EmbedHandlerStack ->
  Sem (EmbedStackWith r) () ->
  UnitTest
testEmbedTrace effs =
  testLogLevel Trace \ conf -> testEmbedConf @r conf effs

-- |Run a plugin test without extra effects and RPC handlers.
testEmbed_ ::
  HasCallStack =>
  Sem EmbedStack () ->
  UnitTest
testEmbed_ =
  testPlugin_ mempty

-- |Run a plugin test without extra effects and RPC handlers.
--
-- Takes a log level, for which the default is to only print critical errors.
testEmbedLevel_ ::
  HasCallStack =>
  Severity ->
  Sem EmbedStack () ->
  UnitTest
testEmbedLevel_ level =
  testEmbedConf @'[] (def & #plugin . #host %~ setStderr level) id

-- |Run a plugin test without extra effects and RPC handlers at the 'Trace' log level for debugging RPC traffic.
testEmbedTrace_ ::
  HasCallStack =>
  Sem EmbedStack () ->
  UnitTest
testEmbedTrace_ =
  testEmbedLevel_ Trace
