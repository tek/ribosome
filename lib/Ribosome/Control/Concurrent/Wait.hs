module Ribosome.Control.Concurrent.Wait where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Default (Default(def))
import UnliftIO.Exception (tryAny)

import Ribosome.Data.Time (sleep)

data Retry =
  Retry Int Double
  deriving Show

instance Default Retry where
  def = Retry 30 0.1

waitIO :: MonadUnliftIO m => Retry -> m a -> (a -> m Bool) -> m (Maybe a)
waitIO (Retry maxRetry interval) thunk cond =
  wait maxRetry
  where
    wait 0 = return Nothing
    wait count = do
      ea <- tryAny thunk
      result <- check ea
      case result of
        Just a -> return $ Just a
        Nothing -> do
          sleep 0.1
          wait (count - 1)
    check (Right a) = do
      ok <- cond a
      return $ if ok then Just a else Nothing
    check _ = return Nothing

waitIODef :: MonadUnliftIO m => m a -> (a -> m Bool) -> m (Maybe a)
waitIODef =
  waitIO def
