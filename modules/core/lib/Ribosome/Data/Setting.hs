module Ribosome.Data.Setting where

data Setting a =
  Setting {
    name :: Text,
    prefix :: Bool,
    fallback :: Maybe a
  }

data GSetting a where
  SettingWithoutDefault :: Text -> Bool -> GSetting a
  SettingWithDefault :: Text -> Bool -> a -> GSetting a
