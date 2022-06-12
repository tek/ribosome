module Ribosome.Interpreter.Settings where

import Exon (exon)

import Ribosome.Data.PluginName (PluginName (PluginName))
import Ribosome.Data.Setting (Setting (Setting))
import qualified Ribosome.Data.SettingError as SettingError
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Effect.Settings (Settings (Get, Update))
import Ribosome.Host.Api.Effect (vimGetVar, vimSetVar)
import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.PluginName (pluginName)

settingVariableName ::
  Member (Reader PluginName) r =>
  Setting a ->
  Sem r Text
settingVariableName = \case
  Setting key False _ ->
    pure key
  Setting key True _ -> do
    PluginName name <- pluginName
    pure [exon|#{name}_#{key}|]

settingRaw ::
  Members [Rpc, Reader PluginName] r =>
  MsgpackDecode a =>
  Setting a ->
  Sem r a
settingRaw s =
  vimGetVar =<< settingVariableName s

fallback ::
  Members [Reader PluginName, Stop SettingError] r =>
  Setting a ->
  Sem r a
fallback = \case
  Setting _ _ (Just a) ->
    pure a
  s@(Setting _ _ Nothing) -> do
    n <- settingVariableName s
    stop (SettingError.Unset n)

interpretSettingsRpc ::
  Members [Rpc !! RpcError, Reader PluginName] r =>
  InterpreterFor (Settings !! SettingError) r
interpretSettingsRpc =
  interpretResumable \case
    Get s ->
      settingRaw s !>> fallback s
    Update s a -> do
      n <- settingVariableName s
      resumeHoist (SettingError.UpdateFailed n) (vimSetVar n a)
