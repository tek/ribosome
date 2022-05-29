module Ribosome.Data.SettingError where

import Log (Severity (Error))

import Ribosome.Host.Data.HandlerError (ErrorMessage (ErrorMessage), ToErrorMessage (toErrorMessage))

newtype SettingError =
  SettingError { unSettingError :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

instance ToErrorMessage SettingError where
  toErrorMessage (SettingError e) =
    ErrorMessage e [e] Error
