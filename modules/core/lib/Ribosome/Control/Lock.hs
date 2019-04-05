module Ribosome.Control.Lock(
  getOrCreateLock,
  lockOrSkip,
) where

import qualified Control.Lens as Lens (at, view)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Data.Map.Strict as Map (insert)
import UnliftIO (finally)
import UnliftIO.STM (TMVar, atomically, newTMVarIO, tryPutTMVar, tryTakeTMVar)

import Ribosome.Control.Monad.Ribo
import Ribosome.Control.Ribosome (Locks)
import qualified Ribosome.Control.Ribosome as Ribosome (locks)
import qualified Ribosome.Log as Log (debugR)

getLocks :: (MonadRibo m, MonadIO m) => m Locks
getLocks =
  pluginInternalL Ribosome.locks

inspectLocks :: (MonadRibo m, MonadIO m) => (Locks -> a) -> m a
inspectLocks = (<$> getLocks)

modifyLocks :: MonadRibo m => (Locks -> Locks) -> m ()
modifyLocks =
  pluginModifyInternal Ribosome.locks

getOrCreateLock :: (MonadRibo m, MonadIO m) => String -> m (TMVar ())
getOrCreateLock key = do
  currentLock <- inspectLocks $ Lens.view $ Lens.at key
  case currentLock of
    Just tv -> return tv
    Nothing -> do
      tv <- newTMVarIO ()
      modifyLocks $ Map.insert key tv
      getOrCreateLock key

lockOrSkip :: (MonadRibo m, MonadIO m, MonadUnliftIO m) => String -> m () -> m ()
lockOrSkip key thunk = do
  currentLock <- getOrCreateLock key
  currentState <- atomically $ tryTakeTMVar currentLock
  case currentState of
    Just _ -> do
      Log.debugR $ "locking MVar `" ++ key ++ "`"
      finally thunk $ atomically $ tryPutTMVar currentLock ()
      Log.debugR $ "unlocking MVar `" ++ key ++ "`"
    Nothing -> return ()