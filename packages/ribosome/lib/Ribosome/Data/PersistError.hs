module Ribosome.Data.PersistError where

import Exon (exon)
import Polysemy.Log (Severity (Error))

import Ribosome.Data.PersistPathError (PersistPathError)
import Ribosome.Host.Data.HandlerError (ErrorMessage (ErrorMessage), ToErrorMessage (toErrorMessage))

data PersistError =
  Permission Text
  |
  Decode Text Text
  |
  Path PersistPathError
  deriving stock (Eq, Show)

instance ToErrorMessage PersistError where
  toErrorMessage = \case
    Permission path ->
      ErrorMessage msg ["PersistError.Permission:", path] Error
      where
        msg =
          [exon|Insufficient permissions for persistence file: #{path}|]
    Decode path err ->
      ErrorMessage msg ["PersistError.Decode:", path, err] Error
      where
        msg =
          [exon|invalid data in persistence file, please delete it: #{path}|]
    Path err ->
      toErrorMessage err
