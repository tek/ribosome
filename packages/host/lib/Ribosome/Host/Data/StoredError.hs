module Ribosome.Host.Data.StoredError where

import qualified Chronos
import Polysemy.Chronos (ChronosTime)
import qualified Time

import Ribosome.Host.Data.HandlerError (ErrorMessage)

data StoredError =
  StoredError {
    error :: ErrorMessage,
    time :: Chronos.Time
  }
  deriving stock (Eq, Show)

now ::
  Member ChronosTime r =>
  ErrorMessage ->
  Sem r StoredError
now e =
  StoredError e <$> Time.now
