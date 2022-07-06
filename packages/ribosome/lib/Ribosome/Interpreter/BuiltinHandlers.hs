module Ribosome.Interpreter.BuiltinHandlers where

import Conc (interpretAtomic, interpretLockReentrant)
import Data.MessagePack (Object)

import Ribosome.Data.Locks (WatcherLock)
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Effect.BuiltinHandlers (BuiltinHandlers (Mapping, Variables))
import qualified Ribosome.Effect.MappingHandler as MappingHandler
import Ribosome.Effect.MappingHandler (MappingHandler)
import Ribosome.Effect.VariableWatcher (VariableWatcher)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.VariableWatcher (variableWatcherHandler)

interpretBuiltinHandlers ::
  ∀ mres r .
  Members [VariableWatcher !! HandlerError, MappingHandler !! HandlerError] r =>
  Members [Rpc !! RpcError, Race, Resource, Mask mres, Embed IO] r =>
  InterpreterFor (BuiltinHandlers !! HandlerError) r
interpretBuiltinHandlers =
  interpretLockReentrant . untag @WatcherLock .
  interpretAtomic (mempty :: Map WatchedVariable Object) .
  interpretResumable \case
    Variables ->
      restop @_ @VariableWatcher variableWatcherHandler
    Mapping i ->
      restop (MappingHandler.call i)
  . raiseUnder2
