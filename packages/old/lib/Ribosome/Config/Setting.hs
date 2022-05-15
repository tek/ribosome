module Ribosome.Config.Setting where

import Ribosome.Data.Setting (Setting(Setting))
import Ribosome.Data.SettingError (SettingError)
import qualified Ribosome.Data.SettingError as SettingError (SettingError(..))
import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode(toMsgpack))
import Ribosome.Host.Api.Effect

settingVariableName ::
  Setting a ->
  m Text
settingVariableName (Setting settingName False _) =
  pure settingName
settingVariableName (Setting settingName True _) = do
  name <- pluginName
  pure $ name <> "_" <> settingName

settingRaw s =
  vimGetVar =<< settingVariableName s

setting ::
  ∀ e m a.
  Member Rpc r =>
  MsgpackDecode a =>
  Setting a ->
  m a
setting s@(Setting n _ fallback') =
  catchAt handleError $ settingRaw s
  where
    handleError (RpcError.Nvim _ _) =
      case fallback' of
        (Just fb) -> pure fb
        Nothing -> throwHoist $ SettingError.Unset n
    handleError a =
      throwHoist a

data SettingOrError =
  Sett SettingError
  |
  Rpc RpcError

deepPrisms ''SettingOrError

settingOr ::
  a ->
  Setting a ->
  m a
settingOr a =
  (fromRight a <$>) . runExceptT . setting @SettingOrError

settingMaybe ::
  Setting a ->
  m (Maybe a)
settingMaybe =
  (rightToMaybe <$>) . runExceptT . setting @SettingOrError

updateSetting ::
  Setting a ->
  a ->
  m ()
updateSetting s a =
  (`vimSetVar` toMsgpack a) =<< settingVariableName s
