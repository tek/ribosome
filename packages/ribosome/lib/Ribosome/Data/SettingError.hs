-- |Error for 'Ribosome.Settings'.
module Ribosome.Data.SettingError where

import Exon (exon)
import Log (Severity (Error))

import Ribosome.Host.Data.Report (Report (Report), Reportable (toReport))
import Ribosome.Host.Data.RpcError (RpcError, rpcError)

-- |The errors emitted by the effect 'Ribosome.Settings'.
data SettingError =
  -- |The variable is unset and has no associated default.
  Unset Text
  |
  -- |The variable contains data that is incompatible with the type parameter of the 'Ribosome.Setting'.
  Decode Text Text
  |
  -- |Something went wrong while attempting to set a variable.
  UpdateFailed Text RpcError
  deriving stock (Eq, Show)

instance Reportable SettingError where
  toReport = \case
    Unset key ->
      Report [exon|Mandatory setting '#{key}' is unset|] ["SettingError.Unset:", key] Error
    Decode key msg ->
      Report [exon|Setting '#{key}' has invalid value: #{msg}|] ["SettingError.Decode:", key, msg] Error
    UpdateFailed key err ->
      Report [exon|Failed to update setting '#{key}': #{rpcError err}|] ["SettingError.UpdateFailed:", key, show err] Error
