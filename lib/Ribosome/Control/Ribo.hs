module Ribosome.Control.Ribo(
  Ribo,
  state,
  swap,
  put,
  inspect,
  modify,
  name,
  prepend,
  modifyL,
  riboInternal,
  getErrors,
  inspectErrors,
  modifyErrors,
) where

import Control.Concurrent.STM.TVar (modifyTVar, swapTVar)
import Control.Lens (Lens')
import qualified Control.Lens as Lens (over)
import Data.Functor (void)
import Neovim (Neovim, ask)
import UnliftIO.STM (TVar, atomically, readTVarIO)

import Ribosome.Control.Ribosome (Ribosome(Ribosome), RibosomeInternal)
import qualified Ribosome.Control.Ribosome as Ribosome (errors, _errors)
import Ribosome.Data.Errors (Errors)

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

modifyL :: Lens' s a -> (a -> a) -> Ribo (TVar s) ()
modifyL lens f =
  modify $ Lens.over lens f

prepend :: Lens' s [a] -> a -> Ribo (TVar s) ()
prepend lens a =
  modifyL lens (a :)

name :: Ribo e String
name = do
  Ribosome n _ _ <- ask
  return n

riboInternal :: Ribo d RibosomeInternal
riboInternal = do
  Ribosome _ intTv _ <- ask
  readTVarIO intTv

getErrors :: Ribo e Errors
getErrors =
  Ribosome.errors <$> riboInternal

inspectErrors :: (Errors -> a) -> Ribo e a
inspectErrors f = fmap f getErrors

modifyErrors :: (Errors -> Errors) -> Ribo e ()
modifyErrors f = do
  Ribosome _ intTv _ <- ask
  atomically $ modifyTVar intTv $ Lens.over Ribosome._errors f
