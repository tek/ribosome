module Ribosome.Test.Data.TestConfig where

import qualified Chiasma.Test.Data.TmuxTestConfig as Chiasma

import Ribosome.Data.PluginConfig (PluginConfig (PluginConfig))
import Ribosome.Host.Data.HostConfig (HostConfig (HostConfig), dataLogConc)

data TestConfig =
  TestConfig {
    freeze :: Bool,
    plugin :: PluginConfig ()
  }
  deriving stock (Eq, Show, Generic)

instance Default TestConfig where
  def =
    TestConfig False (PluginConfig "test" (HostConfig def { dataLogConc = False }) unit)

data TmuxTestConfig =
  TmuxTestConfig {
    core :: TestConfig,
    tmux :: Chiasma.TmuxTestConfig
  }
  deriving stock (Eq, Show, Generic)

instance Default TmuxTestConfig where
  def =
    TmuxTestConfig def def { Chiasma.gui = False }
