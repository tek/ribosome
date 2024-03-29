-- |Main function combinators for remote plugins.
module Ribosome.Remote where

import Ribosome.Data.PluginConfig (PluginConfig)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Interpret (HigherOrder)
import Ribosome.Host.Interpreter.Handlers (interpretHandlersNull, withHandlers)
import Ribosome.Host.Interpreter.Host (runHost)
import Ribosome.Host.Interpreter.Process.Stdio (interpretProcessCerealStdio)
import Ribosome.Host.Run (RpcDeps, RpcStack, interpretRpcStack)
import Ribosome.IOStack (BasicPluginStack, runCli)
import Ribosome.Interpreter.Scratch (interpretScratch)
import Ribosome.Interpreter.Settings (interpretSettingsRpc)
import Ribosome.Interpreter.UserError (interpretUserErrorPrefixed)
import Ribosome.Interpreter.VariableWatcher (interpretVariableWatcherNull)
import Ribosome.Plugin.Builtin (interceptHandlersBuiltin)
import Ribosome.Run (PluginEffects)

-- |The stack of plugin internals.
type HandlerStack =
  PluginEffects ++ RpcStack ++ RpcDeps

-- |The complete stack of a Neovim plugin.
type RemoteStack c =
  HandlerStack ++ BasicPluginStack c

-- |Run plugin internals without IO effects.
interpretPluginRemote ::
  Members (BasicPluginStack c) r =>
  InterpretersFor HandlerStack r
interpretPluginRemote =
  interpretUserErrorPrefixed .
  interpretProcessCerealStdio .
  interpretRpcStack .
  interpretHandlersNull .
  interpretVariableWatcherNull .
  interpretSettingsRpc .
  interpretScratch

-- |Run the main loop for a remote plugin.
remotePlugin ::
  ∀ c r .
  Members HandlerStack r =>
  Members (BasicPluginStack c) r =>
  Sem r ()
remotePlugin =
  interceptHandlersBuiltin runHost

-- |Run plugin internals and IO effects for a remote plugin, reading options from the CLI.
--
-- Like 'runRemoteStack', but allows the CLI option parser to be specified.
runRemoteStackCli ::
  PluginConfig c ->
  Sem (RemoteStack c) () ->
  IO ()
runRemoteStackCli conf =
  runCli conf . interpretPluginRemote

-- |Run plugin internals and IO effects for a remote plugin, reading options from the CLI.
runRemoteStack ::
  PluginConfig () ->
  Sem (RemoteStack ()) () ->
  IO ()
runRemoteStack conf =
  runCli conf . interpretPluginRemote

-- |Run a Neovim plugin using a set of handlers and configuration, with an arbitrary stack of custom effects placed
-- between the handlers and the Ribosome stack.
--
-- Like 'runNvimPluginIO', but allows the 'PluginConfig' to contain a CLI parser for an arbitrary type @c@ that is then
-- provided in a @'Reader' c@ to the plugin.
--
-- This is separate from 'runNvimPluginIO' because it requires a type hint when using @OverloadedStrings@ or 'def' to
-- construct the config without an option parser.
runNvimPluginCli ::
  HigherOrder r (RemoteStack c) =>
  PluginConfig c ->
  InterpretersFor r (RemoteStack c) ->
  [RpcHandler (r ++ RemoteStack c)] ->
  IO ()
runNvimPluginCli conf effs handlers =
  runRemoteStackCli conf (effs (withHandlers handlers remotePlugin))

-- |Run a Neovim plugin using a set of handlers and configuration, with an arbitrary stack of custom effects placed
-- between the handlers and the Ribosome stack.
-- This is important, because the custom effects may want to use the Neovim API, while the handlers want to use both
-- Neovim and the custom effects.
--
-- Example:
--
-- > data Numbers :: Effect where
-- >   Number :: Int -> Sem r Int
-- >
-- > makeSem ''Numbers
-- >
-- > runNumbers :: Member (Rpc !! RpcError) r => InterpreterFor Numbers r
-- > runNumbers = \case
-- >   Number i -> (-1) <! nvimGetVar ("number_" <> show i)
-- >
-- > type CustomStack = [AtomicState Int, Numbers]
-- >
-- > currentNumber :: Handler r Int
-- > currentNumber = number =<< atomicGet
-- >
-- > setNumber :: Int -> Handler r ()
-- > setNumber = atomicPut
-- >
-- > runCustomStack :: InterpretersFor CustomStack r
-- > runCustomStack = interpretAtomic . runNumbers
-- >
-- > main :: IO ()
-- > main = runNvimPluginIO @CustomStack "numbers" runCustomStack [
-- >   rpcFunction "CurrentNumber" Sync currentNumber,
-- >   rpcFunction "SetNumber" Async setNumber
-- >   ]
--
-- /Note/:
--
-- - 'PluginConfig' is being constructed via @OverloadedStrings@
--
-- - @CustomStack@ has to be specified as a type application, because GHC cannot figure it out on its own.
--
-- - For an explanation of @Rpc !! RpcError@, see [Errors]("Ribosome#g:errors")
--
-- This runs the entire stack to completion, so it can be used in the app's @main@ function.
--
-- For more flexibility and less type inference noise, this can be inlined as:
--
-- > runRemoteStack conf $ runCustomStack $ withHandlers handlers remotePlugin
runNvimPluginIO ::
  ∀ r .
  HigherOrder r (RemoteStack ()) =>
  PluginConfig () ->
  InterpretersFor r (RemoteStack ()) ->
  [RpcHandler (r ++ RemoteStack ())] ->
  IO ()
runNvimPluginIO conf effs handlers =
  runRemoteStack conf (effs (withHandlers handlers remotePlugin))

-- |Run a Neovim plugin using a set of handlers and configuration.
--
-- This function does not allow additional effects to be used. See 'runNvimPluginIO' for that purpose.
--
-- This runs the entire stack to completion, so it can be used in the app's @main@ function.
runNvimPluginIO_ ::
  PluginConfig () ->
  [RpcHandler (RemoteStack ())] ->
  IO ()
runNvimPluginIO_ conf handlers =
  runRemoteStack conf (withHandlers handlers remotePlugin)
