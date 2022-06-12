module Ribosome.Data.SettingError where

import Exon (exon)
import Log (Severity (Error))

import Ribosome.Host.Data.HandlerError (ErrorMessage (ErrorMessage), ToErrorMessage (toErrorMessage))
import Ribosome.Host.Data.RpcError (RpcError (RpcError))

data SettingError =
  Unset Text
  |
  UpdateFailed Text RpcError
  deriving stock (Eq, Show)

instance ToErrorMessage SettingError where
  toErrorMessage = \case
    Unset key ->
      ErrorMessage [exon|Mandatory setting '#{key}' is unset|] ["SettingError.Unset:", key] Error
    UpdateFailed key (RpcError err) ->
      ErrorMessage [exon|Failed to update setting '#{key}': #{err}|] ["SettingError.UpdateFailed:", key, err] Error
