module Ribosome.Test.Data.TestConfig where

import Ribosome.Data.PluginConfig (PluginConfig (PluginConfig))

data TestConfig =
  TestConfig {
    freeze :: Bool,
    plugin :: PluginConfig
  }
  deriving stock (Eq, Show)

instance Default TestConfig where
  def =
    TestConfig False (PluginConfig "test" def)
