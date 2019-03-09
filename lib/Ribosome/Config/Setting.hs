module Ribosome.Config.Setting(
  setting,
  updateSetting,
  settingVariableName,
  settingOr,
  settingMaybe,
) where

import Control.Monad.Error.Class (MonadError(catchError))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Either (fromRight)
import Data.Either.Combinators (rightToMaybe)

import Ribosome.Control.Monad.Ribo (MonadRibo, Nvim, pluginName)
import Ribosome.Data.DeepError (MonadDeepError(throwHoist), catchAt)
import Ribosome.Data.Setting (AsSettingError, Setting(Setting), SettingError, _SettingError)
import qualified Ribosome.Data.Setting as SettingError (SettingError(..))
import Ribosome.Msgpack.Decode (MsgpackDecode)
import Ribosome.Msgpack.Encode (MsgpackEncode(toMsgpack))
import Ribosome.Nvim.Api.IO
import Ribosome.Nvim.Api.RpcCall (AsRpcError, RpcError, _RpcError)
import qualified Ribosome.Nvim.Api.RpcCall as RpcError (RpcError(..))

settingVariableName ::
  (MonadRibo m) =>
  Setting a ->
  m String
settingVariableName (Setting settingName False _) =
  return settingName
settingVariableName (Setting settingName True _) = do
  name <- pluginName
  return $ name ++ "_" ++ settingName

settingRaw :: (MonadRibo m, Nvim m, MsgpackDecode a, MonadDeepError e RpcError m) => Setting a -> m a
settingRaw s =
  vimGetVar =<< settingVariableName s

setting ::
  ∀ e m a.
  (MonadIO m, Nvim m, MonadRibo m, MonadDeepError e RpcError m, MonadDeepError e SettingError m, MsgpackDecode a) =>
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

settingOr ::
  (MonadIO m, Nvim m, MonadRibo m, MsgpackDecode a) =>
  a ->
  Setting a ->
  m a
settingOr a =
  (fromRight a <$>) . runExceptT . setting @SettingError

settingMaybe ::
  (MonadIO m, Nvim m, MonadRibo m, MsgpackDecode a) =>
  Setting a ->
  m (Maybe a)
settingMaybe =
  (rightToMaybe <$>) . runExceptT . setting @SettingError

updateSetting ::
  (MonadRibo m, MonadIO m, Nvim m, MonadDeepError e RpcError m, MsgpackEncode a) =>
  Setting a ->
  a ->
  m ()
updateSetting s a =
  (`vimSetVar` toMsgpack a) =<< settingVariableName s
