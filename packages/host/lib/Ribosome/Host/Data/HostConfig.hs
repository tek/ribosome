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
  deriving stock (Eq, Show)

instance Default LogConfig where
  def =
    LogConfig Nothing Info Crit Info

newtype HostConfig =
  HostConfig {
    log :: LogConfig
  }
  deriving stock (Eq, Show)

instance Default HostConfig where
  def =
    HostConfig def
