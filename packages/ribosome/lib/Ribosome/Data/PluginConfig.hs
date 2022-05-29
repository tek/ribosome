module Ribosome.Data.PluginConfig where

import Ribosome.Data.PluginName (PluginName)
import Ribosome.Host.Data.HostConfig (HostConfig)

data PluginConfig =
  PluginConfig {
    name :: PluginName,
    host :: HostConfig
  }
  deriving stock (Eq, Show)
