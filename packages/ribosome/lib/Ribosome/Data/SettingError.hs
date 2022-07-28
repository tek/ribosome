module Ribosome.Data.SettingError where

import Exon (exon)
import Log (Severity (Error))

import Ribosome.Host.Data.Report (Report (Report), Reportable (toReport))
import Ribosome.Host.Data.RpcError (RpcError, rpcReport)

data SettingError =
  Unset Text
  |
  Decode Text Text
  |
  UpdateFailed Text RpcError
  deriving stock (Eq, Show)

instance Reportable SettingError where
  toReport = \case
    Unset key ->
      Report [exon|Mandatory setting '#{key}' is unset|] ["SettingError.Unset:", key] Error
    Decode key msg ->
      Report [exon|Setting '#{key}' has invalid value: #{msg}|] ["SettingError.Decode:", key, msg] Error
    UpdateFailed key err ->
      Report [exon|Failed to update setting '#{key}': #{rpcReport err}|] ["SettingError.UpdateFailed:", key, show err] Error
