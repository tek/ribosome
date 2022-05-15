module Ribosome.Control.Exception where

import Control.Exception.Lifted (IOException, catch, try)

tryIO ::
  m a ->
  m (Either IOException a)
tryIO =
  try

tryAny ::
  m a ->
  m (Either SomeException a)
tryAny =
  try

catchAny ::
  (SomeException -> m a) ->
  m a ->
  m a
catchAny =
  flip catch

catchAnyAs ::
  a ->
  m a ->
  m a
catchAnyAs =
  catchAny . const . pure
