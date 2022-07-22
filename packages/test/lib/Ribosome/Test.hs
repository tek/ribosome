module Ribosome.Test (
  -- * Introduction
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
  TestEffects,
  TestConfig (TestConfig),
  TmuxTestConfig (TmuxTestConfig),
  -- * Error handling
  module Ribosome.Test.Error,
  module Ribosome.Host.Data.HandlerError,
  -- * Assertions for Neovim UI elements
  module Ribosome.Test.Ui,
  -- * Assertions that are made repeatedly until the succeed
  module Ribosome.Test.Wait,
) where

import Ribosome.Host.Data.HandlerError (resumeHandlerFail, stopHandlerToFail)
import Ribosome.Test.Data.TestConfig (TestConfig (TestConfig), TmuxTestConfig (TmuxTestConfig))
import Ribosome.Test.Embed (
  EmbedStack,
  EmbedStackWith,
  TestEffects,
  runEmbedTest,
  runTest,
  runTestConf,
  runTestLogConf,
  testEmbed,
  testEmbedConf,
  testEmbedLevel,
  testEmbedLevel_,
  testEmbedTrace,
  testEmbedTrace_,
  testEmbed_,
  testPlugin,
  testPluginConf,
  testPluginEmbed,
  testPlugin_,
  )
import Ribosome.Test.Error
import Ribosome.Test.Ui
import Ribosome.Test.Wait

-- $intro
-- This is the test library for the "Ribosome" Neoivm plugin framework.
--
-- Three different test environments are available:
--
-- - "Ribosome.Test.Embed" runs Neovim as a subprocess and connects over stdio
--
-- - "Ribosome.Test.EmbedTmux" is like "Ribosome.Test.Embed", but provides a tmux server
--
-- - "Ribosome.Test.SocketTmux" runs Neovim in a window in a fresh tmux server, either headless in a pseudo terminal
-- or in an @xterm@ instance
--
-- This module reexports "Ribosome.Test.Embed".
--
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
