module Ribosome.Test (
  -- * Introduction
  -- $intro

  -- * Embedded testing
  -- $embed

  -- * tmux testing
  -- $tmux

  -- * Embedded test API
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
  testEmbedDebug,
  testEmbedDebug_,
  testEmbedTrace,
  testEmbedTrace_,
  runTestConf,
  runTestLogConf,
  EmbedStackWith,
  EmbedStack,
  EmbedHandlerStack,
  TestEffects,
  TestConfig (TestConfig),
  TmuxTestConfig (TmuxTestConfig),
  -- * Error handling
  module Ribosome.Test.Error,
  module Ribosome.Host.Data.Report,
  -- * Assertions for Neovim UI elements
  windowCountIs,
  cursorIs,
  currentCursorIs,
  awaitScreenshot,
  -- * Assertions that are made repeatedly until the succeed
  assertWait,
  assertWaitFor,
) where

import Ribosome.Host.Data.Report (resumeReportFail, stopReportToFail)
import Ribosome.Test.Data.TestConfig (TestConfig (TestConfig), TmuxTestConfig (TmuxTestConfig))
import Ribosome.Test.Embed (
  EmbedHandlerStack,
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
  testEmbedDebug,
  testEmbedTrace_,
  testEmbedDebug_,
  testEmbed_,
  testPlugin,
  testPluginConf,
  testPluginEmbed,
  testPlugin_,
  )
import Ribosome.Test.Error
import Ribosome.Test.Screenshot (awaitScreenshot)
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

-- $embed
-- Running a test against an embedded Neovim process is the simplest approach that is suited for unit testing plugin
-- logic where the integration with Neovim startup isn't important.
--
-- Handlers can be registered in Neovim and triggered via RPC API functions like 'nvimCallFunction' and 'nvimCommand'.
-- Most of the time this is only interesting if a handler has complex parameters and you want to test that they are
-- decoded correctly, or that the handler is triggered properly by an autocmd.
-- In more basic cases, where only the interaction with Neovim from within the handler is relevant, it can simply be run
-- directly.
--
-- > import Polysemy.Test
-- > import Ribosome
-- > import Ribosome.Api
-- > import Ribosome.Test
-- >
-- > store ::
-- >   Member (Rpc !! RpcError) r =>
-- >   Args ->
-- >   Handler r ()
-- > store (Args msg) =
-- >   ignoreRpcError do
-- >     nvimSetVar "message" msg
-- >
-- > test_direct :: UnitTest
-- > test_direct =
-- >   testEmbed_ do
-- >     store "test directly"
-- >     assertEq "test directly" =<< nvimGetVar @Text "message"
-- >
-- > test_rpc :: UnitTest
-- > test_rpc =
-- >   testPlugin_ [rpcCommand "Store" Sync store] do
-- >     nvimCommand "Store test RPC"
-- >     assertEq "test RPC" =<< nvimGetVar @Text "message"
--
-- See [Ribosome.Test.Embed]("Ribosome.Test.Embed") for more options.

-- $tmux
-- It is possible to run a standalone Neovim instance to test against.
-- This is useful to observe the UI's behaviour for debugging purposes, but might also be desired to test a feature in
-- the full environment that is used in production.
--
-- Ribosome provides a testing mode that starts a terminal with a tmux server, in which Neovim is executed as a regular
-- shell process.
-- Variants of this that run tmux in a pseudo terminal that is not rendered, or simply run a tmux server for use in an
-- embedded test, are also available.
--
-- In the terminal case, the test connects the plugin over a socket.
-- It is possible to take \"screenshots\" (capturing the tmux pane running Neovim) that are automatically stored in the
-- @fixtures@ directory of the test suite and compared to previous recordings on subsequent runs, as in this example
-- that runs tmux in a terminal and tests some syntax rules:
--
-- > import Polysemy.Test
-- > import Ribosome.Api
-- > import Ribosome.Syntax
-- > import Ribosome.Test
-- > import Ribosome.Test.SocketTmux
-- >
-- > syntax :: Syntax
-- > syntax =
-- >   Syntax [syntaxMatch "TestColons" "::"] [
-- >     syntaxHighlight "TestColons" [("cterm", "reverse"), ("ctermfg", "1"), ("gui", "reverse"), ("guifg", "#dc322f")]
-- >   ] []
-- >
-- > test_syntax :: UnitTest
-- > test_syntax =
-- >   testSocketTmuxGui do
-- >     setCurrentBufferContent ["function :: String -> Int", "function _ = 5"]
-- >     _ <- executeSyntax syntax
-- >     awaitScreenshot False "syntax" 0
-- >
-- > See [Ribosome.Test.SocketTmux]("Ribosome.Test.SocketTmux") and [Ribosome.Test.EmbedTmux]("Ribosome.Test.EmbedTmux")
-- > for more options.
