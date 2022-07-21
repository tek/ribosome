module Ribosome.Run where

import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Effect.VariableWatcher (VariableWatcher)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Handlers (Handlers)
import Ribosome.Host.Effect.Rpc (Rpc)

type PluginEffects =
  [
    Scratch !! RpcError,
    Settings !! SettingError,
    VariableWatcher !! HandlerError,
    Handlers !! HandlerError
  ]

-- |The set of core effects that handlers and API functions commonly use.
type NvimPlugin =
  [
    Scratch !! RpcError,
    Settings !! SettingError,
    Rpc !! RpcError,
    Reader PluginName
  ]
