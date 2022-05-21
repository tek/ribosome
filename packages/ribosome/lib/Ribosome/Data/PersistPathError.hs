module Ribosome.Data.PersistPathError where

import Polysemy.Log (Severity (Error))

import Ribosome.Host.Data.HandlerError (ErrorMessage (ErrorMessage), ToErrorMessage (toErrorMessage))

data PersistPathError =
  Undefined
  deriving stock (Eq, Show)

instance ToErrorMessage PersistPathError where
  toErrorMessage Undefined =
    ErrorMessage msg ["PersistPathError.Undefined"] Error
    where
      msg =
        "g:ribosome_persistence_dir unset and XDG not available."
