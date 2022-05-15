module Ribosome.Control.Lock where

import Control.Exception.Lifted (finally)
import qualified Control.Lens as Lens (at, view)
import qualified Data.Map.Strict as Map (insert)

import Ribosome.Control.Ribosome (Locks)
import qualified Ribosome.Control.Ribosome as Ribosome (locks)

getLocks =
  pluginInternalL Ribosome.locks

inspectLocks = (<$> getLocks)

modifyLocks =
  pluginInternalModifyL Ribosome.locks

getOrCreateLock key = do
  currentLock <- inspectLocks $ Lens.view $ Lens.at key
  case currentLock of
    Just tv -> pure tv
    Nothing -> do
      tv <- newTMVarIO ()
      modifyLocks $ Map.insert key tv
      getOrCreateLock key

lockOrSkip ::
  Text ->
  m () ->
  m ()
lockOrSkip key thunk = do
  currentLock <- getOrCreateLock key
  currentState <- atomically $ tryTakeTMVar currentLock
  case currentState of
    Just _ -> do
      Log.debug $ "locking TMVar `" <> key <> "`"
      finally thunk $ atomically $ tryPutTMVar currentLock ()
      Log.debug $ "unlocking TMVar `" <> key <> "`"
    Nothing -> pure ()

lockOrWait ::
  Text ->
  m () ->
  m ()
lockOrWait key thunk = do
  currentLock <- getOrCreateLock key
  atomically $ takeTMVar currentLock
  Log.debug $ "locking TMVar `" <> key <> "`"
  finally thunk $ atomically $ putTMVar currentLock ()
  Log.debug $ "unlocking TMVar `" <> key <> "`"
