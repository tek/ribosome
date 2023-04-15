-- |The configuration for a Ribosome plugin host.
module Ribosome.Host.Data.HostConfig where

import Log (Severity (Crit, Info))
import Path (Abs, File, Path)

-- |Logging config for a host, with different levels for Neovim echoing, stderr and file logs.
--
-- /Note/ that stderr logging will be sent to Neovim when the plugin is running in remote mode, which will be ignored
-- unless the plugin is started with a stderr handler.
data LogConfig =
  LogConfig {
    logFile :: Maybe (Path Abs File),
    logLevelEcho :: Severity,
    logLevelStderr :: Severity,
    logLevelFile :: Severity,
    dataLogConc :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance Default LogConfig where
  def =
    LogConfig Nothing Info Crit Info True

-- |The configuration for a host, which consists only of a 'LogConfig'.
newtype HostConfig =
  HostConfig {
    hostLog :: LogConfig
  }
  deriving stock (Eq, Show, Generic)

instance Default HostConfig where
  def =
    HostConfig def

-- |Set the stderr level on a 'HostConfig'.
setStderr :: Severity -> HostConfig -> HostConfig
setStderr l c =
  c { hostLog = c.hostLog { logLevelStderr = l } }
