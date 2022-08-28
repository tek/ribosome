-- |Data type abstracting a Neovim variable with a default value.
module Ribosome.Data.Setting where

-- |This type is used by the effect 'Ribosome.Settings', representing a Neovim variable associated with a plugin.
--
-- It has a name, can optionally prefixed by the plugin's name and may define a default value that is used when the
-- variable is undefined.
--
-- The type parameter determines how the Neovim value is decoded.
data Setting a =
  Setting {
    key :: Text,
    prefix :: Bool,
    fallback :: Maybe a
  }
