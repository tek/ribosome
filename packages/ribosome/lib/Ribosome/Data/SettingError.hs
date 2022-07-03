module Ribosome.Data.SettingError where

import Exon (exon)
import Log (Severity (Error))

import Ribosome.Host.Data.HandlerError (ErrorMessage (ErrorMessage), ToErrorMessage (toErrorMessage))
import Ribosome.Host.Data.RpcError (RpcError, rpcErrorMessage)

data SettingError =
  Unset Text
  |
  Decode Text Text
  |
  UpdateFailed Text RpcError
  deriving stock (Eq, Show)

instance ToErrorMessage SettingError where
  toErrorMessage = \case
    Unset key ->
      ErrorMessage [exon|Mandatory setting '#{key}' is unset|] ["SettingError.Unset:", key] Error
    Decode key msg ->
      ErrorMessage [exon|Setting '#{key}' has invalid value: #{msg}|] ["SettingError.Decode:", key, msg] Error
    UpdateFailed key err ->
      ErrorMessage [exon|Failed to update setting '#{key}': #{rpcErrorMessage err}|] ["SettingError.UpdateFailed:", key, show err] Error
