module Ribosome.VariableWatcher where

import qualified Data.Map.Strict as Map
import Data.MessagePack (Object)

import Ribosome.Data.Locks (WatcherLock)
import Ribosome.Data.WatchedVariable (WatchedVariable (WatchedVariable))
import qualified Ribosome.Effect.VariableWatcher as VariableWatcher
import Ribosome.Effect.VariableWatcher (VariableWatcher)
import Ribosome.Host.Api.Effect (nvimGetVar)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Locks (lockOrSkip)

compareVar ::
  Members [VariableWatcher, AtomicState (Map WatchedVariable Object)] r =>
  WatchedVariable ->
  Object ->
  Maybe Object ->
  Sem r ()
compareVar var new = \case
  Just old | old == new ->
    unit
  _ -> do
    atomicModify' (Map.insert var new)
    VariableWatcher.update new var

checkVar ::
  Members [VariableWatcher, AtomicState (Map WatchedVariable Object), Rpc !! RpcError] r =>
  WatchedVariable ->
  Sem r ()
checkVar var = do
  old <- atomicGets (Map.lookup var)
  resume_ do
    new <- nvimGetVar (coerce var)
    compareVar var new old

variableWatcherHandler ::
  Member (AtomicState (Map WatchedVariable Object)) r =>
  Members [VariableWatcher, Rpc !! RpcError, Sync WatcherLock, Resource, Stop HandlerError] r =>
  Sem r ()
variableWatcherHandler =
  void $ lockOrSkip @WatcherLock do
    traverse_ checkVar =<< VariableWatcher.watchedVariables
