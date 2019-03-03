module Ribosome.Config.Setting(
  setting,
  updateSetting,
  settingVariableName,
  settingOr,
  settingMaybe,
) where

import qualified Control.Lens as Lens (preview, review)
import Control.Monad.Error.Class (MonadError(throwError, catchError))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Either (fromRight)
import Data.Either.Combinators (rightToMaybe)

import Ribosome.Control.Monad.Ribo (MonadRibo, Nvim, pluginName)
import Ribosome.Data.Setting (AsSettingError, Setting(Setting), SettingError, _SettingError)
import qualified Ribosome.Data.Setting as SettingError (SettingError(..))
import Ribosome.Msgpack.Decode (MsgpackDecode)
import Ribosome.Msgpack.Encode (MsgpackEncode(toMsgpack))
import Ribosome.Nvim.Api.IO
import Ribosome.Nvim.Api.RpcCall (AsRpcError, _RpcError)
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

settingRaw :: (MonadRibo m, MonadError e m, AsRpcError e, Nvim m, MsgpackDecode a) => Setting a -> m a
settingRaw s =
  vimGetVar =<< settingVariableName s

setting ::
  ∀ e m a.
  (MonadIO m, Nvim m, MonadRibo m, MonadError e m, AsRpcError e, AsSettingError e, MsgpackDecode a) =>
  Setting a ->
  m a
setting s@(Setting n _ fallback') =
  (`catchError` handleError) $ settingRaw s
  where
    handleError a =
      case Lens.preview (_SettingError . _RpcError) a of
        Just (RpcError.Nvim _) ->
          case fallback' of
            (Just fb) -> return fb
            Nothing -> throwError $ Lens.review _SettingError $ SettingError.Unset n
        _ ->
          throwError a

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
  (MonadRibo m, MonadError e m, MonadIO m, Nvim m, AsRpcError e, MsgpackEncode a) =>
  Setting a ->
  a ->
  m ()
updateSetting s a =
  (`vimSetVar` toMsgpack a) =<< settingVariableName s
