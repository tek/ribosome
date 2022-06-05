module Ribosome.Run where

import Data.MessagePack (Object)

import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Effect.BuiltinHandlers (BuiltinHandlers)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (Handler)
import Ribosome.Host.IOStack (BasicStack)
import Ribosome.Host.Run (RpcStack)
import Ribosome.Interpreter.BuiltinHandlers (interpretBuiltinHandlers)
import Ribosome.Interpreter.Scratch (interpretScratch)
import Ribosome.Interpreter.Settings (interpretSettingsRpc)

type PluginEffects =
  [
    Scratch !! RpcError,
    Settings !! SettingError
  ]

interpretPluginEffects ::
  ∀ r r' .
  r' ~ PluginEffects ++ r =>
  Members BasicStack r =>
  Members RpcStack r =>
  Member (Reader PluginName) r =>
  Map MappingIdent (Handler r' ()) ->
  Map WatchedVariable (Object -> Handler r' ()) ->
  InterpretersFor (BuiltinHandlers !! HandlerError : PluginEffects) r
interpretPluginEffects maps vars =
  interpretSettingsRpc .
  interpretScratch .
  interpretBuiltinHandlers maps vars
