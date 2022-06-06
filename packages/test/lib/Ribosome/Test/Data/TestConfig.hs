module Ribosome.Test.Data.TestConfig where

import Chiasma.Test.Tmux (TmuxTestConf (ttcGui))

import Ribosome.Data.PluginConfig (PluginConfig (PluginConfig))

data TestConfig =
  TestConfig {
    freeze :: Bool,
    plugin :: PluginConfig
  }
  deriving stock (Eq, Show, Generic)

instance Default TestConfig where
  def =
    TestConfig False (PluginConfig "test" def)

data TmuxTestConfig =
  TmuxTestConfig {
    core :: TestConfig,
    tmux :: TmuxTestConf
  }
  deriving stock (Eq, Show, Generic)

instance Default TmuxTestConfig where
  def =
    TmuxTestConfig def def { ttcGui = False }
