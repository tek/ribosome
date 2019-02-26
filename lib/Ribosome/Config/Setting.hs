module Ribosome.Config.Setting(
  Setting (..),
  setting,
  settingE,
  updateSetting,
  settingVariableName,
  settingOr,
  settingMaybe,
  settingR,
  SettingError(..),
  updateSettingR,
) where

import Data.Either (fromRight)
import Data.Text.Prettyprint.Doc (Doc)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import Neovim
import System.Log (Priority(NOTICE))

import Ribosome.Control.Monad.RiboE (RiboE, liftRibo, riboE, mapE)
import Ribosome.Control.Ribo (Ribo)
import qualified Ribosome.Control.Ribosome as R (name)
import Ribosome.Data.ErrorReport (ErrorReport(..))
import Ribosome.Error.Report (ReportError(..))
import Ribosome.Msgpack.Decode (MsgpackDecode(fromMsgpack))
import Ribosome.Msgpack.Encode (MsgpackEncode(toMsgpack))

data SettingError =
  Other String String
  |
  Decode String (Doc AnsiStyle)
  |
  Unset String
  deriving Show

instance ReportError SettingError where
  errorReport (Other name message) =
    ErrorReport ("weird setting: " ++ name) ["failed to read setting `" ++ name ++ "`", message] NOTICE
  errorReport (Decode name message) =
    ErrorReport ("invalid setting: " ++ name) ["failed to decode setting `" ++ name ++ "`", show message] NOTICE
  errorReport (Unset name) =
    ErrorReport ("required setting unset: " ++ name) ["unset setting: `" ++ name ++ "`"] NOTICE

data Setting a =
  Setting {
    name :: String,
    prefix :: Bool,
    fallback :: Maybe a
  }

settingVariableName :: Setting a -> Ribo e String
settingVariableName (Setting n False _) = return n
settingVariableName (Setting n True _) = do
  pluginName <- R.name <$> ask
  return $ pluginName ++ "_" ++ n

settingE :: NvimObject a => Setting a -> Ribo e (Either String a)
settingE s@(Setting _ _ fallback') = do
  varName <- settingVariableName s
  raw <- vim_get_var varName
  case raw of
    Right o -> fromObject' o
    Left a -> return $ case fallback' of
      Just fb -> Right fb
      Nothing -> Left $ show a

setting :: NvimObject a => Setting a -> Ribo e a
setting s = do
  raw <- settingE s
  either fail return raw

settingOr :: NvimObject a => a -> Setting a -> Ribo e a
settingOr a s = do
  raw <- settingE s
  return $ fromRight a raw

settingMaybe :: NvimObject a => Setting a -> Ribo e (Maybe a)
settingMaybe s = do
  raw <- settingE s
  return $ either (const Nothing) Just raw

updateSetting :: NvimObject a => Setting a -> a -> Ribo e ()
updateSetting s a = do
  varName <- settingVariableName s
  _ <- vim_set_var' varName (toObject a)
  return ()

settingR :: MsgpackDecode a => Setting a -> RiboE s SettingError a
settingR s@(Setting n _ fallback') = do
  varName <- liftRibo $ settingVariableName s
  raw <- liftRibo $ vim_get_var varName
  case raw of
    Right o -> mapE (Decode n) $ riboE $ pure $ fromMsgpack o
    Left _ -> riboE $ return $ case fallback' of
      Just fb -> Right fb
      Nothing -> Left $ Unset n

updateSettingR :: MsgpackEncode a => Setting a -> a -> Ribo e ()
updateSettingR s a = do
  varName <- settingVariableName s
  void $ vim_set_var' varName (toMsgpack a)
