module Ribosome.Control.Exception where

import Control.Exception.Lifted (IOException, catch, try)

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

catchAny ::
  MonadBaseControl IO m =>
  (SomeException -> m a) ->
  m a ->
  m a
catchAny =
  flip catch

catchAnyAs ::
  MonadBaseControl IO m =>
  a ->
  m a ->
  m a
catchAnyAs =
  catchAny . const . return
