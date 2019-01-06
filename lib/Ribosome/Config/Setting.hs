module Ribosome.Config.Setting(
  Setting (..),
  setting,
  settingE,
  updateSetting,
  settingVariableName,
  settingOr,
  settingMaybe,
) where

import Data.Either (fromRight)
import Neovim
import Ribosome.Control.Ribo (Ribo)
import qualified Ribosome.Control.Ribosome as R (name)

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
