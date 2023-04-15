-- |Global configuration for a Ribosome plugin.
module Ribosome.Data.PluginConfig where

import Exon (exon)
import Options.Applicative (Parser)

import Ribosome.Data.PluginName (PluginName)
import Ribosome.Host.Data.HostConfig (HostConfig)

-- |The full configuration for a Ribosome plugin, consisting of the 'HostConfig', the plugin's name, and an arbitrary
-- type for additional config defined by individual plugins.
data PluginConfig c =
  PluginConfig {
    name :: PluginName,
    host :: HostConfig,
    custom :: Parser c
  }
  deriving stock (Generic)

instance Show (PluginConfig c) where
  showsPrec d PluginConfig {..} =
    showParen (d > 10) [exon|PluginConfing { name = #{showsPrec 11 name}, host = #{showsPrec 11 host} }|]

instance Eq (PluginConfig c) where
  PluginConfig ln lh _ == PluginConfig rn rh _ =
    ln == rn && lh == rh

-- |Construct a simple 'PluginConfig' with the default config for the host, given the plugin's name.
pluginNamed :: PluginName -> PluginConfig ()
pluginNamed name =
  PluginConfig name def unit

instance IsString (PluginConfig ()) where
  fromString =
    pluginNamed . fromString
