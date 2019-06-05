module Ribosome.Control.Lock where

import Control.Exception.Lifted (finally)
import qualified Control.Lens as Lens (at, view)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Map as Map (insert)
import UnliftIO.STM (TMVar, newTMVarIO, putTMVar, takeTMVar, tryPutTMVar, tryTakeTMVar)

import Ribosome.Control.Monad.Ribo (MonadRibo, pluginInternalL, pluginInternalModifyL)
import Ribosome.Control.Ribosome (Locks)
import qualified Ribosome.Control.Ribosome as Ribosome (locks)
import qualified Ribosome.Log as Log (debug)

getLocks :: (MonadRibo m, MonadIO m) => m Locks
getLocks =
  pluginInternalL Ribosome.locks

inspectLocks :: (MonadRibo m, MonadIO m) => (Locks -> a) -> m a
inspectLocks = (<$> getLocks)

modifyLocks :: MonadRibo m => (Locks -> Locks) -> m ()
modifyLocks =
  pluginInternalModifyL Ribosome.locks

getOrCreateLock :: (MonadRibo m, MonadIO m) => Text -> m (TMVar ())
getOrCreateLock key = do
  currentLock <- inspectLocks $ Lens.view $ Lens.at key
  case currentLock of
    Just tv -> return tv
    Nothing -> do
      tv <- newTMVarIO ()
      modifyLocks $ Map.insert key tv
      getOrCreateLock key

lockOrSkip ::
  MonadRibo m =>
  MonadIO m =>
  MonadBaseControl IO m =>
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
    Nothing -> return ()

lockOrWait ::
  MonadRibo m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  Text ->
  m () ->
  m ()
lockOrWait key thunk = do
  currentLock <- getOrCreateLock key
  atomically $ takeTMVar currentLock
  Log.debug $ "locking TMVar `" <> key <> "`"
  finally thunk $ atomically $ putTMVar currentLock ()
  Log.debug $ "unlocking TMVar `" <> key <> "`"
