{-# options_haddock prune #-}

-- |Main function combinators for embedding Neovim
module Ribosome.Embed where

import Ribosome.Data.PluginConfig (PluginConfig)
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Host.Data.BootError (BootError)
import Ribosome.Host.Data.HostConfig (LogConfig)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Embed (EmbedExtra, interpretEmbedExtra)
import Ribosome.Host.IOStack (IOStack)
import Ribosome.Host.Interpret (HigherOrder)
import Ribosome.Host.Interpreter.Handlers (interpretHandlersNull, withHandlers)
import Ribosome.Host.Interpreter.Host (withHost)
import Ribosome.Host.Interpreter.Process.Embed (interpretProcessCerealNvimEmbed)
import Ribosome.Host.Run (RpcDeps, RpcStack, interpretRpcStack)
import Ribosome.IOStack (BasicPluginStack, runCli)
import Ribosome.Interpreter.Scratch (interpretScratch)
import Ribosome.Interpreter.Settings (interpretSettingsRpc)
import Ribosome.Interpreter.UserError (interpretUserErrorPrefixed)
import Ribosome.Interpreter.VariableWatcher (interpretVariableWatcherNull)
import Ribosome.Plugin.Builtin (interceptHandlersBuiltin)
import Ribosome.Run (PluginEffects)

type HandlerEffects =
  PluginEffects ++ RpcStack ++ EmbedExtra ++ RpcDeps

-- |The complete stack for an embedded plugin test.
type EmbedStack =
  HandlerEffects ++ BasicPluginStack

interpretRpcDeps ::
  Members [Reader PluginName, Error BootError, Log, Resource, Race, Async, Embed IO] r =>
  InterpretersFor RpcDeps r
interpretRpcDeps =
  interpretUserErrorPrefixed .
  interpretProcessCerealNvimEmbed Nothing Nothing

-- |Run the internal stack for an embedded Neovim test, without IO effects.
interpretPluginEmbed ::
  Members [Log, Reader LogConfig, Reader PluginName] r =>
  Members IOStack r =>
  InterpretersFor HandlerEffects r
interpretPluginEmbed =
  interpretRpcDeps .
  interpretEmbedExtra .
  interpretRpcStack .
  interpretHandlersNull .
  interpretVariableWatcherNull .
  interpretSettingsRpc .
  interpretScratch

-- |Fork the main loop for a plugin connected to an embedded Neovim.
embedPlugin ::
  Members BasicPluginStack r =>
  Members HandlerEffects r =>
  Sem r a ->
  Sem r a
embedPlugin =
  interceptHandlersBuiltin .
  withHost .
  insertAt @0

-- |Run an embedded Neovim, plugin internals and IO effects, reading options from the CLI.
runEmbedStack ::
  PluginConfig ->
  Sem EmbedStack () ->
  IO ()
runEmbedStack conf =
  runCli conf . interpretPluginEmbed

-- |Run a 'Sem' in an embedded plugin context by starting a Neovim subprocess, forking the Ribosome main loop and
-- registering the supplied handlers, using the supplied custom effect stack.
--
-- This is a basic version of what
-- [ribosome-test](https://hackage.haskell.org/package/ribosome-test/docs/Ribosome-Test.html) provides, which uses
-- [polysemy-test](https://hackage.haskell.org/package/polysemy-test/docs/Polysemy-Test.html) and
-- [hedgehog](https://hackage.haskell.org/package/hedgehog) for a comprehensive testing framework.
--
-- The parameters have the same meaning as for [remote plugins]("Ribosome#execution").
runEmbedPluginIO ::
  HigherOrder r EmbedStack =>
  PluginConfig ->
  InterpretersFor r EmbedStack ->
  [RpcHandler (r ++ EmbedStack)] ->
  Sem (r ++ EmbedStack) () ->
  IO ()
runEmbedPluginIO conf effs handlers =
  runEmbedStack conf .
  effs .
  withHandlers handlers .
  embedPlugin

-- |Run a 'Sem' in an embedded plugin context by starting a Neovim subprocess, forking the Ribosome main loop and
-- registering the supplied handlers.
--
-- Like 'runEmbedPluginIO', but without extra effects.
runEmbedPluginIO_ ::
  PluginConfig ->
  [RpcHandler EmbedStack] ->
  Sem EmbedStack () ->
  IO ()
runEmbedPluginIO_ conf handlers =
  runEmbedStack conf .
  withHandlers handlers .
  embedPlugin
