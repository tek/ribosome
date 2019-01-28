{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}

module Ribosome.Control.Monad.Ribo(
  Ribo,
  state,
  put,
  RiboT(..),
  MonadRibo(..),
  unsafeToNeovim,
) where

import Control.Concurrent.STM.TVar (swapTVar)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT, mapExceptT)
import qualified Control.Monad.Trans.Except as Except (catchE)
import Data.Bifunctor (Bifunctor(..))
import Data.Either.Combinators (mapLeft)
import Data.Functor (void)
import UnliftIO.Exception (throwString)
import UnliftIO.STM (TVar, atomically, readTVarIO)
import Neovim (ask)
import Neovim.Context.Internal (Neovim(Neovim))
import Ribosome.Control.Ribosome (Ribosome)
import qualified Ribosome.Control.Ribosome as Ribosome (env)

type Ribo e = Neovim (Ribosome e)

newtype RiboT s e a =
  RiboT { unRiboT :: ExceptT e (Neovim (Ribosome (TVar s))) a }
  deriving (Functor, Applicative, Monad)

instance Bifunctor (RiboT s) where
  first f (RiboT r) =
    RiboT $ mapExceptT (fmap $ mapLeft f) r

  second = fmap

class Monad (t s e) => MonadRibo s e t where
  nvim :: Neovim (Ribosome (TVar s)) a -> t s e a
  asNeovim :: t s e a -> Neovim (Ribosome (TVar s)) (Either e a)
  liftEither :: (Either e a) -> t s e a
  catchE :: (e -> t s e' a) -> t s e a -> t s e' a

instance MonadRibo s e RiboT where
  nvim = RiboT . ExceptT . fmap Right
  asNeovim = runExceptT . unRiboT
  liftEither = RiboT . ExceptT . return
  catchE f = RiboT . (flip Except.catchE) (unRiboT . f) . unRiboT

instance (Monad (t s e), MonadRibo s e t) => MonadError e (t s e) where
  throwError = liftEither . Left
  catchError = flip catchE

stateTVar :: (MonadRibo s e t) => t s e (TVar s)
stateTVar =
  Ribosome.env <$> nvim ask

instance (MonadRibo s e t) => MonadState s (t s e) where
  get = do
    t <- stateTVar
    nvim $ readTVarIO t
  put newState = do
    t <- stateTVar
    void $ nvim $ atomically $ swapTVar t newState

unsafeToNeovim :: (MonadRibo s e t, Show e) => t s e a -> Neovim (Ribosome (TVar s)) a
unsafeToNeovim ra = do
  r <- asNeovim ra
  either (throwString . show) return r
