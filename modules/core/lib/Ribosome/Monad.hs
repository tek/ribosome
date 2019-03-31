module Ribosome.Monad(
  liftExceptTIO,
  liftExceptT,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT(ExceptT))

liftExceptTIO :: (MonadIO m) => IO a -> ExceptT e m a
liftExceptTIO fa = ExceptT $ Right <$> liftIO fa

liftExceptT :: (Functor m) => m a -> ExceptT e m a
liftExceptT fa = ExceptT $ Right <$> fa
