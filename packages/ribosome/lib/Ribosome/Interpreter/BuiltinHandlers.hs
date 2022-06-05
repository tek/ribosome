module Ribosome.Interpreter.BuiltinHandlers where

import Data.MessagePack (Object)
import Polysemy.Conc (interpretAtomic, interpretSyncAs)

import Ribosome.Data.Locks (WatcherLock (WatcherLock))
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Effect.BuiltinHandlers (BuiltinHandlers (Mapping, Variables))
import qualified Ribosome.Effect.MappingHandler as MappingHandler
import Ribosome.Effect.NvimPlugin (NvimPlugin')
import Ribosome.Effect.VariableWatcher (VariableWatcher)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.VariableWatcher (variableWatcherHandler)

interpretBuiltinHandlers ::
  ∀ r .
  Members NvimPlugin' r =>
  Members [Rpc !! RpcError, Race, Resource, Embed IO] r =>
  InterpreterFor (BuiltinHandlers !! HandlerError) r
interpretBuiltinHandlers =
  interpretSyncAs WatcherLock .
  interpretAtomic (mempty :: Map WatchedVariable Object) .
  interpretResumable \case
    Variables ->
      restop @_ @VariableWatcher variableWatcherHandler
    Mapping i ->
      restop (MappingHandler.call i)
  . raiseUnder2
