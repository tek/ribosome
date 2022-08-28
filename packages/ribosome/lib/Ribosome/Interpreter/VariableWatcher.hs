-- |Interpreters for 'VariableWatcher'
module Ribosome.Interpreter.VariableWatcher where

import Conc (interpretAtomic, interpretLockReentrant, lockOrSkip_)
import qualified Data.Map.Strict as Map
import Data.MessagePack (Object (ObjectNil))

import Ribosome.Effect.VariableWatcher (WatchedVariable (WatchedVariable))
import qualified Ribosome.Effect.VariableWatcher as VariableWatcher
import Ribosome.Effect.VariableWatcher (VariableWatcher)
import Ribosome.Host.Api.Effect (nvimGetVar)
import Ribosome.Host.Data.Report (Report)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (Handler)
import Ribosome.Host.Effect.Rpc (Rpc)

-- |Interpret 'VariableWatcher' by doing nothing.
interpretVariableWatcherNull :: InterpreterFor (VariableWatcher !! Report) r
interpretVariableWatcherNull =
  interpretResumable \case
    VariableWatcher.Update -> unit
    VariableWatcher.Unwatch _ -> unit

-- |Run the handler if the two 'Object's are different.
runIfDifferent ::
  (Object -> Handler r ()) ->
  Object ->
  Object ->
  Handler r ()
runIfDifferent handler new old =
  unless (old == new) (handler new)

-- |Fetch the current value of the watched variable and call the handler if its value has changed.
checkVar ::
  Member (Rpc !! RpcError) r =>
  WatchedVariable ->
  Object ->
  (Object -> Handler r ()) ->
  Handler r Object
checkVar (WatchedVariable var) old handler =
  resumeAs old do
    new <- nvimGetVar var
    new <$ raise (runIfDifferent handler new old)

-- |This is a reactive system that is triggered by several frequently sent autocommands to inspect a user-defined set of
-- Neovim variables for changes.
-- When a variable's value has been observed to have changed from the previously recorded state, the associated handler
-- is executed.
--
-- This handler has to be passed to 'Ribosome.runNvimPluginIO' or similar as part of the custom effect stack, like:
--
-- > runNvimPluginIO "my-plugin" (watchVariables [("variable_name", handler)]) mempty
--
-- This does not remove 'VariableWatcher' from the stack, but intercepts and resends it, to make it simpler to use
-- with the plugin runners.
watchVariables ::
  Members [VariableWatcher !! Report, Rpc !! RpcError, Resource, Mask mres, Race, Embed IO] r =>
  Map WatchedVariable (Object -> Handler r ()) ->
  Sem r a ->
  Sem r a
watchVariables vars =
  interpretLockReentrant .
  interpretAtomic ((ObjectNil,) <$> vars) .
  interceptResumable \case
    VariableWatcher.Update -> do
      lockOrSkip_ do
        atomicPut =<< Map.traverseWithKey (\ var (old, h) -> (,h) <$> raiseUnder2 (checkVar var old h)) =<< atomicGet
      restop @Report VariableWatcher.update
    VariableWatcher.Unwatch var -> do
      atomicModify' (Map.delete var)
      restop @Report (VariableWatcher.unwatch var)
  . raise . raise

-- |Interpret 'VariableWatcher' with 'watchVariables', but eliminate the effect from the stack.
interpretVariableWatcher ::
  Members [Rpc !! RpcError, Resource, Mask mres, Race, Embed IO] r =>
  Map WatchedVariable (Object -> Handler (VariableWatcher !! Report : r) ()) ->
  InterpreterFor (VariableWatcher !! Report) r
interpretVariableWatcher vars =
  interpretVariableWatcherNull . watchVariables vars
