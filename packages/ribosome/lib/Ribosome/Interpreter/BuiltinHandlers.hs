module Ribosome.Interpreter.BuiltinHandlers where

import Data.MessagePack (Object)
import Polysemy.Conc (interpretAtomic, interpretSyncAs)

import Ribosome.Data.Locks (WatcherLock (WatcherLock))
import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Effect.BuiltinHandlers (BuiltinHandlers (Mapping, Variable))
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (Handler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Mapping (mappingHandler)
import Ribosome.VariableWatcher (variableWatcherHandler)

type VarEffects =
  [
    AtomicState (Map WatchedVariable Object),
    Sync WatcherLock
  ]

interpretBuiltinHandlers ::
  Members [Rpc !! RpcError, Race, Resource, Embed IO] r =>
  Map MappingIdent (Handler r ()) ->
  Map WatchedVariable (Object -> Handler r ()) ->
  InterpreterFor (BuiltinHandlers !! HandlerError) r
interpretBuiltinHandlers maps vars =
  interpretSyncAs WatcherLock .
  interpretAtomic (mempty :: Map WatchedVariable Object) .
  interpretResumable \case
    Variable ->
      variableWatcherHandler ((raiseUnder2 .) <$> vars)
    Mapping i ->
      mappingHandler (raiseUnder2 <$> maps) i
  . raiseUnder2
