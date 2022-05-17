module Ribosome.Data.Setting where

data Setting a =
  Setting {
    key :: Text,
    prefix :: Bool,
    fallback :: Maybe a
  }
