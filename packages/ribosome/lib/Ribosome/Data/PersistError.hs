module Ribosome.Data.PersistError where

import Exon (exon)
import Polysemy.Log (Severity (Error))

import Ribosome.Data.PersistPathError (PersistPathError)
import Ribosome.Host.Data.HandlerError (HandlerError (HandlerError), ToHandlerError (toHandlerError))

data PersistError =
  Permission Text
  |
  Decode Text Text
  |
  Path PersistPathError
  deriving stock (Eq, Show)

instance ToHandlerError PersistError where
  toHandlerError = \case
    Permission path ->
      HandlerError msg ["PersistError.Permission:", path] Error
      where
        msg =
          [exon|Insufficient permissions for persistence file: #{path}|]
    Decode path err ->
      HandlerError msg ["PersistError.Decode:", path, err] Error
      where
        msg =
          [exon|invalid data in persistence file, please delete it: #{path}|]
    Path err ->
      toHandlerError err
