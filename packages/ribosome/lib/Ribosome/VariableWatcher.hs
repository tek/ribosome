module Ribosome.VariableWatcher where

import qualified Data.Map.Strict as Map
import Data.MessagePack (Object)

import Ribosome.Data.Locks (WatcherLock)
import Ribosome.Data.WatchedVariable (WatchedVariable (WatchedVariable))
import Ribosome.Host.Api.Effect (nvimGetVar)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Locks (lockOrSkip)

compareVar ::
  Member (AtomicState (Map WatchedVariable Object)) r =>
  WatchedVariable ->
  (Object -> Sem r ()) ->
  Object ->
  Maybe Object ->
  Sem r ()
compareVar var handler new = \case
  Just old | old == new ->
    unit
  _ -> do
    atomicModify' (Map.insert var new)
    handler new

checkVar ::
  Members [AtomicState (Map WatchedVariable Object), Rpc !! RpcError] r =>
  WatchedVariable ->
  (Object -> Sem r ()) ->
  Sem r ()
checkVar var handler = do
  old <- atomicGets (Map.lookup var)
  resume_ do
    new <- nvimGetVar (coerce var)
    compareVar var (raise . handler) new old

variableWatcherHandler ::
  Member (AtomicState (Map WatchedVariable Object)) r =>
  Members [Rpc !! RpcError, Sync WatcherLock, Resource, Stop HandlerError] r =>
  Map WatchedVariable (Object -> Sem r ()) ->
  Sem r ()
variableWatcherHandler vars =
  void $ lockOrSkip @WatcherLock do
    Map.traverseWithKey checkVar vars
