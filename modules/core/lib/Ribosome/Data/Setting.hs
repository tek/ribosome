module Ribosome.Data.Setting where

data Setting a =
  Setting {
    name :: Text,
    prefix :: Bool,
    fallback :: Maybe a
  }
