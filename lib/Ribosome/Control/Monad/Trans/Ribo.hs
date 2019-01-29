{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ribosome.Control.Monad.Trans.Ribo where

import Control.Concurrent.STM.TVar (swapTVar)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT, mapExceptT)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.Trans.Except as Except (catchE)
import Data.Bifunctor (Bifunctor(..))
import Data.Either.Combinators (mapLeft)
import Data.Functor (void)
import UnliftIO.Exception (throwString)
import UnliftIO.STM (TVar, atomically, readTVarIO)
import Neovim (ask)
import Neovim.Context.Internal (Neovim)
import Ribosome.Control.Ribosome (Ribosome)
import qualified Ribosome.Control.Ribosome as Ribosome (env)

type Ribo e = Neovim (Ribosome (TVar e))

newtype RiboT t s e a =
  RiboT { unRiboT :: ExceptT e (t (Ribo s)) a }

deriving instance Functor (t (Ribo s)) => Functor (RiboT t s e)
deriving instance Monad (t (Ribo s)) => Applicative (RiboT t s e)
deriving instance Monad (t (Ribo s)) => Monad (RiboT t s e)
deriving instance MonadIO (t (Ribo s)) => MonadIO (RiboT t s e)
deriving instance Monad (t (Ribo s)) => MonadError e (RiboT t s e)

nvim :: MonadTrans t => Neovim (Ribosome (TVar s)) a -> RiboT t s e a
nvim = RiboT . ExceptT . lift . fmap Right

asNeovim :: RiboT t s e a -> t (Ribo s) (Either e a)
asNeovim = runExceptT . unRiboT

asNeovimWith :: (∀ b. t (Ribo s) b -> Ribo s b) -> RiboT t s e a -> Ribo s (Either e a)
asNeovimWith run = run . runExceptT . unRiboT

mapE :: (Functor (t (Ribo s))) => (e -> e') -> RiboT t s e a -> RiboT t s e' a
mapE f =
  RiboT . trans . unRiboT
  where
    trans = mapExceptT (fmap $ mapLeft f)

liftEither :: (Monad (t (Ribo s))) => Either e a -> RiboT t s e a
liftEither = RiboT . ExceptT . return

catchE :: (Monad (t (Ribo s))) => (e -> RiboT t s e' a) -> RiboT t s e a -> RiboT t s e' a
catchE f = RiboT . flip Except.catchE (unRiboT . f) . unRiboT

stateTVar :: (MonadTrans t, Functor (t (Ribo s))) => RiboT t s e (TVar s)
stateTVar =
  Ribosome.env <$> nvim ask

toException :: Show e => Ribo s (Either e a) -> Ribo s a
toException ma = ma >>= either (throwString . show) return

unsafeToNeovim :: (MonadTrans t, Monad (t (Ribo s)), Show e) => RiboT t s e a -> t (Ribo s) a
unsafeToNeovim ra = do
  r <- asNeovim ra
  lift $ either (throwString . show) return r

unsafeToNeovimWith :: Show e => (∀ b. t (Ribo s) b -> Ribo s b) -> RiboT t s e a -> Ribo s a
unsafeToNeovimWith run =
  toException . run . asNeovim

instance (MonadTrans t, Monad (t (Ribo s))) => MonadState s (RiboT t s e) where
  get = do
    t <- stateTVar
    nvim $ readTVarIO t
  put newState = do
    t <- stateTVar
    void $ nvim $ atomically $ swapTVar t newState

-- instance (Monad (t (Ribo s))) => MonadError e (RiboT t s e) where
--   throwError = liftEither . Left
--   catchError = flip catchE

instance (Functor (t (Ribo s))) => Bifunctor (RiboT t s) where
  first = mapE
  second = fmap
