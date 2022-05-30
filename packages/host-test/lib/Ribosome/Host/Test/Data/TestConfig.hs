module Ribosome.Host.Test.Data.TestConfig where

import Ribosome.Host.Data.HostConfig (HostConfig)

data TestConfig =
  TestConfig {
    freezeTime :: Bool,
    host :: HostConfig
  }
  deriving stock (Eq, Show)

instance Default TestConfig where
  def =
    TestConfig False def
