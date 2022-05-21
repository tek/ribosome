module Ribosome.Data.PersistPathError where

import Polysemy.Log (Severity (Error))

import Ribosome.Host.Data.HandlerError (HandlerError (HandlerError), ToHandlerError (toHandlerError))

data PersistPathError =
  Undefined
  deriving stock (Eq, Show)

instance ToHandlerError PersistPathError where
  toHandlerError Undefined =
    HandlerError "No persistence path defined and XDG not available." ["PersistPathError.Undefined"] Error
