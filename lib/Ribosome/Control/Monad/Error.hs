module Ribosome.Control.Monad.Error(
  recoverAs,
  recoveryFor,
) where

import Control.Monad.Error.Class (MonadError(catchError))

recoveryFor :: MonadError e m => m a -> m a -> m a
recoveryFor = flip catchError . const

recoverAs :: MonadError e m => m a -> m a -> m a
recoverAs = flip recoveryFor
