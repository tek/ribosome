module Ribosome.Interpreter.VariableWatcher where

import Conc (interpretAtomic, interpretLockReentrant, lockOrSkip_)
import qualified Data.Map.Strict as Map
import Data.MessagePack (Object (ObjectNil))

import Ribosome.Data.WatchedVariable (WatchedVariable (WatchedVariable))
import qualified Ribosome.Effect.VariableWatcher as VariableWatcher
import Ribosome.Effect.VariableWatcher (VariableWatcher)
import Ribosome.Host.Api.Effect (nvimGetVar)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (Handler)
import Ribosome.Host.Effect.Rpc (Rpc)

interpretVariableWatcherNull :: InterpreterFor (VariableWatcher !! HandlerError) r
interpretVariableWatcherNull =
  interpretResumable \case
    VariableWatcher.Update -> unit
    VariableWatcher.Unwatch _ -> unit

runIfDifferent ::
  (Object -> Handler r ()) ->
  Object ->
  Object ->
  Handler r ()
runIfDifferent handler new old =
  unless (old == new) (handler new)

checkVar ::
  Member (Rpc !! RpcError) r =>
  WatchedVariable ->
  Object ->
  (Object -> Handler r ()) ->
  Handler r Object
checkVar (WatchedVariable var) old handler = do
  resumeAs old do
    new <- nvimGetVar var
    new <$ raise (runIfDifferent handler new old)

interpretVariableWatcher ::
  Members [Rpc !! RpcError, Resource, Mask mres, Race, Embed IO] r =>
  Map WatchedVariable (Object -> Handler r ()) ->
  InterpreterFor (VariableWatcher !! HandlerError) r
interpretVariableWatcher vars =
  interpretLockReentrant .
  interpretAtomic ((ObjectNil,) <$> vars) .
  interpretResumable \case
    VariableWatcher.Update ->
      lockOrSkip_ do
        atomicPut =<< Map.traverseWithKey (\ var (old, h) -> (,h) <$> raiseUnder2 (checkVar var old h)) =<< atomicGet
    VariableWatcher.Unwatch var ->
      atomicModify' (Map.delete var)
  . raiseUnder2
