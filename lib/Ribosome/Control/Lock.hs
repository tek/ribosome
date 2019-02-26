module Ribosome.Control.Lock(
  getOrCreateLock,
  lockOrSkip,
) where

import qualified Control.Lens as Lens (view, over, at)
import qualified Data.Map.Strict as Map (insert)
import Neovim (ask)
import UnliftIO (finally)
import UnliftIO.STM (TMVar, atomically, newTMVarIO, tryTakeTMVar, tryPutTMVar, modifyTVar)

import Ribosome.Control.Ribo (Ribo, riboInternal)
import Ribosome.Control.Ribosome (Ribosome(Ribosome), Locks)
import qualified Ribosome.Control.Ribosome as Ribosome (_locks, locks)
import qualified Ribosome.Log as Log (debugR)

getLocks :: Ribo e Locks
getLocks =
  Ribosome.locks <$> riboInternal

inspectLocks :: (Locks -> a) -> Ribo e a
inspectLocks f = fmap f getLocks

modifyLocks :: (Locks -> Locks) -> Ribo e ()
modifyLocks f = do
  Ribosome _ intTv _ <- ask
  atomically $ modifyTVar intTv $ Lens.over Ribosome._locks f

getOrCreateLock :: String -> Ribo e (TMVar ())
getOrCreateLock key = do
  currentLock <- inspectLocks $ Lens.view $ Lens.at key
  case currentLock of
    Just tv -> return tv
    Nothing -> do
      tv <- newTMVarIO ()
      modifyLocks $ Map.insert key tv
      getOrCreateLock key

lockOrSkip :: String -> Ribo e () -> Ribo e ()
lockOrSkip key thunk = do
  currentLock <- getOrCreateLock key
  currentState <- atomically $ tryTakeTMVar currentLock
  case currentState of
    Just _ -> do
      Log.debugR $ "locking MVar `" ++ key ++ "`"
      finally thunk $ atomically $ tryPutTMVar currentLock ()
      Log.debugR $ "unlocking MVar `" ++ key ++ "`"
    Nothing -> return ()
