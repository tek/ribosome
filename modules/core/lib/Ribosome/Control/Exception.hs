module Ribosome.Control.Exception where

import Control.Exception.Lifted (IOException, try)
import Control.Monad.Trans.Control (MonadBaseControl)

tryIO ::
  MonadBaseControl IO m =>
  m a ->
  m (Either IOException a)
tryIO =
  try

tryAny ::
  MonadBaseControl IO m =>
  m a ->
  m (Either SomeException a)
tryAny =
  try
