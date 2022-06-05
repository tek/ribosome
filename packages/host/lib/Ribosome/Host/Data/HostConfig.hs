module Ribosome.Host.Data.HostConfig where

import Log (Severity (Crit, Info))
import Path (Abs, File, Path)

data LogConfig =
  LogConfig {
    logFile :: Maybe (Path Abs File),
    logLevelEcho :: Severity,
    logLevelStderr :: Severity,
    logLevelFile :: Severity
  }
  deriving stock (Eq, Show, Generic)

instance Default LogConfig where
  def =
    LogConfig Nothing Info Crit Info

newtype HostConfig =
  HostConfig {
    hostLog :: LogConfig
  }
  deriving stock (Eq, Show, Generic)

instance Default HostConfig where
  def =
    HostConfig def

setStderr :: Severity -> HostConfig -> HostConfig
setStderr l c =
  c { hostLog = (hostLog c) { logLevelStderr = l } }
