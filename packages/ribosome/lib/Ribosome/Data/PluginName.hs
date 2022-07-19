-- |Data type 'PluginName'
module Ribosome.Data.PluginName where

-- |Represents the name of the plugin, to be used via 'Reader' by all its components.
--
-- The name is usually provided by main function combinators like 'Ribosome.runNvimPluginIO' via @'Reader'
-- 'Ribosome.PluginConfig'@.
newtype PluginName =
  PluginName { unPluginName :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)
