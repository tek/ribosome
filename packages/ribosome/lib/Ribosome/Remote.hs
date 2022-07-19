module Ribosome.Remote where

import Ribosome.Data.PluginConfig (PluginConfig)
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Effect.NvimPlugin (NvimPlugin)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.IOStack (IOStack)
import Ribosome.Host.Interpret (HigherOrder)
import Ribosome.Host.Interpreter.Host (runHost)
import Ribosome.Host.Interpreter.Process.Stdio (interpretProcessCerealStdio)
import Ribosome.Host.Run (RpcDeps, RpcStack, interpretRpcStack)
import Ribosome.IOStack (BasicPluginStack, runCli)
import Ribosome.Interpreter.BuiltinHandlers (interpretBuiltinHandlers)
import Ribosome.Interpreter.NvimPlugin (rpcHandlers, sendNvimPlugin)
import Ribosome.Interpreter.Scratch (interpretScratch)
import Ribosome.Interpreter.Settings (interpretSettingsRpc)
import Ribosome.Interpreter.UserError (interpretUserErrorPrefixed)
import Ribosome.Plugin.Builtin (interceptHandlersBuiltin)
import Ribosome.Run (PluginEffects)

type HandlerStack =
  PluginEffects ++ RpcStack ++ RpcDeps

-- |The complete stack of a Neovim plugin.
type RemoteStack =
  HandlerStack ++ BasicPluginStack

interpretRpcDeps ::
  Members IOStack r =>
  Member (Reader PluginName) r =>
  InterpretersFor RpcDeps r
interpretRpcDeps =
  interpretUserErrorPrefixed .
  interpretProcessCerealStdio

interpretPluginRemote ::
  Members BasicPluginStack r =>
  InterpretersFor HandlerStack r
interpretPluginRemote =
  interpretRpcDeps .
  interpretRpcStack .
  interpretSettingsRpc .
  interpretScratch

runPluginHostRemote ::
  ∀ r .
  Member NvimPlugin r =>
  Members HandlerStack r =>
  Members BasicPluginStack r =>
  Sem r ()
runPluginHostRemote =
  sendNvimPlugin $
  interpretBuiltinHandlers $
  interceptHandlersBuiltin runHost

runNvimPlugin ::
  ∀ r .
  HigherOrder (NvimPlugin : r) RemoteStack =>
  InterpretersFor (NvimPlugin : r) RemoteStack ->
  Sem BasicPluginStack ()
runNvimPlugin handlers =
  interpretPluginRemote (handlers runPluginHostRemote)

runNvimPlugin_ ::
  InterpreterFor NvimPlugin RemoteStack ->
  Sem BasicPluginStack ()
runNvimPlugin_ =
  runNvimPlugin @'[]

runNvimHandlers ::
  ∀ r .
  HigherOrder r RemoteStack =>
  InterpretersFor r RemoteStack ->
  [RpcHandler (r ++ RemoteStack)] ->
  Sem BasicPluginStack ()
runNvimHandlers effs handlers =
  runNvimPlugin @r (effs . rpcHandlers handlers)

runNvimHandlers_ ::
  [RpcHandler RemoteStack] ->
  Sem BasicPluginStack ()
runNvimHandlers_ =
  runNvimHandlers @'[] id

-- |Run a neovim plugin using a chain of interpreters that eliminate both 'NvimPlugin' and an arbitrary stack of custom
-- effects.
-- This provides more functionality than 'runNvimHandlersIO', since 'NvimPlugin' not only interprets basic request
-- handlers, but also mappings and watched variables.
runNvimPluginIO ::
  ∀ r .
  HigherOrder (NvimPlugin : r) RemoteStack =>
  PluginConfig ->
  InterpretersFor (NvimPlugin : r) RemoteStack ->
  IO ()
runNvimPluginIO conf handlers =
  runCli conf (runNvimPlugin @r handlers)

runNvimPluginIO_ ::
  PluginConfig ->
  InterpreterFor NvimPlugin RemoteStack ->
  IO ()
runNvimPluginIO_ =
  runNvimPluginIO @'[]

-- |Run a Neovim plugin using a set of handlers and configuration, with an arbitrary stack of custom effects placed
-- between the handlers and the Ribosome stack.
-- This is important, because the custom effects may want to use the Neovim API, while the handlers want to use both
-- Neovim and the custom effects.
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
-- > main :: IO ()
-- > main = runNvimHandlersIO @CustomStack "numbers" (interpretAtomic . runNumbers) [
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
runNvimHandlersIO ::
  ∀ r .
  HigherOrder r RemoteStack =>
  PluginConfig ->
  InterpretersFor r RemoteStack ->
  [RpcHandler (r ++ RemoteStack)] ->
  IO ()
runNvimHandlersIO conf effs handlers =
  runCli conf (runNvimHandlers @r effs handlers)

-- |Run a Neovim plugin using a set of handlers and configuration.
-- This function does not allow additional effects to be used. See 'runNvimHandlersIO' for that purpose.
--
-- This runs the entire stack to completion, so it can be used in the app's @main@ function.
runNvimHandlersIO_ ::
  PluginConfig ->
  [RpcHandler RemoteStack] ->
  IO ()
runNvimHandlersIO_ conf =
  runNvimHandlersIO @'[] conf id
