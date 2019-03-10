module Ribosome.Control.Monad.DeepError where

import Control.Monad.Error.Class (MonadError(throwError, catchError))

import Ribosome.Data.DeepPrisms (DeepPrisms, hoist, retrieve)

class (MonadError e m, DeepPrisms e e') => MonadDeepError e e' m where
  throwHoist :: e' -> m a

instance (MonadError e m, DeepPrisms e e') => MonadDeepError e e' m where
  throwHoist =
    throwError . hoist

catchAt :: (MonadError e m, DeepPrisms e e') => (e' -> m a) -> m a -> m a
catchAt handle ma =
  catchError ma f
  where
    f e = maybe (throwError e) handle (retrieve e)

hoistEither :: MonadDeepError e e' m => Either e' a -> m a
hoistEither =
  either throwHoist return

hoistMaybe :: MonadDeepError e e' m => e' -> Maybe a -> m a
hoistMaybe e' =
  maybe (throwHoist e') return
