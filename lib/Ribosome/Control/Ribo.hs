module Ribosome.Control.Ribo(
  Ribo,
  state,
  swap,
  put,
  inspect,
  modify,
  name,
  lockOrSkip,
  riboInternal,
  getErrors,
  inspectErrors,
  modifyErrors,
) where

import Control.Concurrent.STM.TVar (modifyTVar, swapTVar)
import qualified Control.Lens as Lens (view, over, at)
import Data.Functor (void)
import qualified Data.Map.Strict as Map (insert)
import UnliftIO (finally)
import UnliftIO.STM (TVar, TMVar, atomically, readTVarIO, newTMVarIO, tryTakeTMVar, tryPutTMVar)
import Neovim (Neovim, ask)
import Ribosome.Control.Ribosome (Ribosome(Ribosome), Locks)
import qualified Ribosome.Control.Ribosome as Ribosome (_locks, locks)

type Ribo e = Neovim (Ribosome e)

state :: Ribo (TVar e) e
state = do
  Ribosome _ _ t <- ask
  readTVarIO t

swap :: e -> Ribo (TVar e) e
swap newState = do
  Ribosome _ _ t <- ask
  atomically $ swapTVar t newState

put :: e -> Ribo (TVar e) ()
put = void . swap

inspect :: (e -> a) -> Ribo (TVar e) a
inspect f = fmap f state

modify :: (e -> e) -> Ribo (TVar e) ()
modify f = do
  Ribosome _ _ t <- ask
  atomically $ modifyTVar t f

name :: Ribo e String
name = do
  Ribosome n _ _ <- ask
  return n

riboInternal :: Ribo d RibosomeInternal
riboInternal = do
  Ribosome _ intTv _ <- ask
  readTVarIO intTv

getLocks :: Ribo e Locks
getLocks =
  Ribosome.locks <$> riboInternal

inspectLocks :: (Locks -> a) -> Ribo e a
inspectLocks f = fmap f getLocks

modifyLocks :: (Locks -> Locks) -> Ribo e ()
modifyLocks f = do
  Ribosome _ intTv _ <- ask
  atomically $ modifyTVar intTv $ Lens.over Ribosome._locks f

getErrors :: Ribo e Errors
getErrors =
  Ribosome.errors <$> riboInternal

inspectErrors :: (Errors -> a) -> Ribo e a
inspectErrors f = fmap f getErrors

modifyErrors :: (Errors -> Errors) -> Ribo e ()
modifyErrors f = do
  Ribosome _ intTv _ <- ask
  atomically $ modifyTVar intTv $ Lens.over Ribosome._errors f

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
    Just _ -> finally thunk $ atomically $ tryPutTMVar currentLock ()
    Nothing -> return ()
