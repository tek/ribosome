module Ribosome.Host.Test.Data.TestConfig where

import Ribosome.Host.Data.HostConfig (HostConfig (HostConfig), dataLogConc)

data TestConfig =
  TestConfig {
    freezeTime :: Bool,
    host :: HostConfig
  }
  deriving stock (Eq, Show, Generic)

instance Default TestConfig where
  def =
    TestConfig False (HostConfig def { dataLogConc = False })
