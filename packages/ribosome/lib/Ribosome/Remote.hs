-- |Main function combinators for remote plugins
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
type RemoteStack =
  HandlerStack ++ BasicPluginStack

-- |Run plugin internals without IO effects.
interpretPluginRemote ::
  Members BasicPluginStack r =>
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
  ∀ r .
  Members HandlerStack r =>
  Members BasicPluginStack r =>
  Sem r ()
remotePlugin =
  interceptHandlersBuiltin runHost

-- |Run plugin internals and IO effects for a remote plugin, reading options from the CLI.
runRemoteStack ::
  PluginConfig ->
  Sem RemoteStack () ->
  IO ()
runRemoteStack conf =
  runCli conf . interpretPluginRemote

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
-- - For an explanation of @Rpc !! RpcError@, see [Errors]("Ribosome#errors")
--
-- This runs the entire stack to completion, so it can be used in the app's @main@ function.
--
-- For more flexibility and less type inference noise, this can be inlined as:
--
-- > runRemoteStack conf $ runCustomStack $ withHandlers handlers remotePlugin
runNvimPluginIO ::
  HigherOrder r RemoteStack =>
  PluginConfig ->
  InterpretersFor r RemoteStack ->
  [RpcHandler (r ++ RemoteStack)] ->
  IO ()
runNvimPluginIO conf effs handlers =
  runRemoteStack conf (effs (withHandlers handlers remotePlugin))

-- |Run a Neovim plugin using a set of handlers and configuration.
--
-- This function does not allow additional effects to be used. See 'runNvimHandlersIO' for that purpose.
--
-- This runs the entire stack to completion, so it can be used in the app's @main@ function.
runNvimPluginIO_ ::
  PluginConfig ->
  [RpcHandler RemoteStack] ->
  IO ()
runNvimPluginIO_ conf handlers =
  runRemoteStack conf (withHandlers handlers remotePlugin)
