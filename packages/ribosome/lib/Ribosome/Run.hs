module Ribosome.Run where

import Ribosome.Data.SettingError (SettingError)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Effect.VariableWatcher (VariableWatcher)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcError (RpcError)

type PluginEffects =
  [
    Scratch !! RpcError,
    Settings !! SettingError,
    VariableWatcher !! HandlerError
  ]
