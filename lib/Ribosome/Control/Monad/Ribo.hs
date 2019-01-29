{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Ribosome.Control.Monad.Ribo(
  Ribo,
  RiboT(..),
  MonadRibo(..),
  MonadRiboError(..),
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

class MonadRibo s e m | m -> s, m -> e where
  nvim :: Neovim (Ribosome (TVar s)) a -> m a
  asNeovim :: m a -> Neovim (Ribosome (TVar s)) (Either e a)

class Monad (t e) => MonadRiboError e t where
  liftEither :: Either e a -> t e a
  mapE :: (e -> e') -> t e a -> t e' a
  catchE :: (e -> t e' a) -> t e a -> t e' a

instance MonadRibo s e (RiboT s e) where
  nvim = RiboT . ExceptT . fmap Right
  asNeovim = runExceptT . unRiboT

instance MonadRiboError e (RiboT s) where
  liftEither = RiboT . ExceptT . return
  mapE f = RiboT . mapExceptT (fmap $ mapLeft f) . unRiboT
  catchE f = RiboT . flip Except.catchE (unRiboT . f) . unRiboT

instance MonadError e (RiboT s e) where
  throwError = liftEither . Left
  catchError = flip catchE

stateTVar :: (Functor m, MonadRibo s e m) => m (TVar s)
stateTVar =
  Ribosome.env <$> nvim ask

instance MonadState s (RiboT s e) where
  get = do
    t <- stateTVar
    nvim $ readTVarIO t
  put newState = do
    t <- stateTVar
    void $ nvim $ atomically $ swapTVar t newState

unsafeToNeovim :: (MonadRibo s e m, Show e) => m a -> Neovim (Ribosome (TVar s)) a
unsafeToNeovim ra = do
  r <- asNeovim ra
  either (throwString . show) return r
