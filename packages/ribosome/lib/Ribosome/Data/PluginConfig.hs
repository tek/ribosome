module Ribosome.Data.PluginConfig where

import Ribosome.Data.PluginName (PluginName)
import Ribosome.Host.Data.HostConfig (HostConfig)

data PluginConfig =
  PluginConfig {
    name :: PluginName,
    host :: HostConfig
  }
  deriving stock (Eq, Show, Generic)

pluginNamed :: PluginName -> PluginConfig
pluginNamed name =
  PluginConfig name def

instance IsString PluginConfig where
  fromString =
    pluginNamed . fromString
