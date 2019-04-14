{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Config.Setting where

import Control.Monad.DeepError (MonadDeepError(throwHoist), catchAt)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.DeepPrisms (deepPrisms)

import Ribosome.Control.Monad.Ribo (MonadRibo, Nvim, pluginName)
import Ribosome.Data.Setting (Setting(Setting))
import Ribosome.Data.SettingError (SettingError)
import qualified Ribosome.Data.SettingError as SettingError (SettingError(..))
import Ribosome.Msgpack.Decode (MsgpackDecode)
import Ribosome.Msgpack.Encode (MsgpackEncode(toMsgpack))
import Ribosome.Nvim.Api.IO
import Ribosome.Nvim.Api.RpcCall (RpcError)
import qualified Ribosome.Nvim.Api.RpcCall as RpcError (RpcError(..))

settingVariableName ::
  (MonadRibo m) =>
  Setting a ->
  m Text
settingVariableName (Setting settingName False _) =
  return settingName
settingVariableName (Setting settingName True _) = do
  name <- pluginName
  return $ name <> "_" <> settingName

settingRaw :: (MonadRibo m, Nvim m, MsgpackDecode a, MonadDeepError e RpcError m) => Setting a -> m a
settingRaw s =
  vimGetVar =<< settingVariableName s

setting ::
  ∀ e m a.
  (Nvim m, MonadRibo m, MonadDeepError e RpcError m, MonadDeepError e SettingError m, MsgpackDecode a) =>
  Setting a ->
  m a
setting s@(Setting n _ fallback') =
  catchAt handleError $ settingRaw s
  where
    handleError (RpcError.Nvim _) =
      case fallback' of
        (Just fb) -> return fb
        Nothing -> throwHoist $ SettingError.Unset n
    handleError a =
      throwHoist a

data SettingOrError =
  Sett SettingError
  |
  Rpc RpcError

deepPrisms ''SettingOrError

settingOr ::
  (MonadIO m, Nvim m, MonadRibo m, MsgpackDecode a) =>
  a ->
  Setting a ->
  m a
settingOr a =
  (fromRight a <$>) . runExceptT . setting @SettingOrError

settingMaybe ::
  (MonadIO m, Nvim m, MonadRibo m, MsgpackDecode a) =>
  Setting a ->
  m (Maybe a)
settingMaybe =
  (rightToMaybe <$>) . runExceptT . setting @SettingOrError

updateSetting ::
  (MonadRibo m, MonadIO m, Nvim m, MonadDeepError e RpcError m, MsgpackEncode a) =>
  Setting a ->
  a ->
  m ()
updateSetting s a =
  (`vimSetVar` toMsgpack a) =<< settingVariableName s
