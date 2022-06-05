module Ribosome.Run where

import Ribosome.Data.SettingError (SettingError)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.RpcError (RpcError)

type PluginEffects =
  [
    Scratch !! RpcError,
    Settings !! SettingError
  ]
