module Ribosome.Control.Concurrent.Wait where

import Control.Exception.Lifted (SomeException(..), try)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Default (Default(def))

import Ribosome.System.Time (sleep)

data Retry =
  Retry Int Double
  deriving Show

instance Default Retry where
  def = Retry 30 0.1

waitIO :: (MonadIO m, MonadBaseControl IO m) => Retry -> m a -> (a -> m Bool) -> m (Either String a)
waitIO (Retry maxRetry interval) thunk cond =
  wait maxRetry (Left "initial")
  where
    wait 0 reason = return reason
    wait count _ = do
      ea <- try thunk
      result <- check ea
      case result of
        Right a -> return $ Right a
        Left reason -> do
          sleep interval
          wait (count - 1) (Left reason)
    check (Right a) = do
      ok <- cond a
      return $ if ok then Right a else Left "condition unmet"
    check (Left (SomeException _)) =
      return $ Left "exception"

waitIODef :: (MonadIO m, MonadBaseControl IO m) => m a -> (a -> m Bool) -> m (Either String a)
waitIODef =
  waitIO def
