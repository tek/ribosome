-- |Config data types for the CLI option parser.
module Ribosome.Data.CliConfig where

import Log (Severity)
import Path (Abs, File, Path)

-- |Intermediate data type with optional fields for 'Ribosome.LogConfig'.
data CliLogConfig =
  CliLogConfig {
    logFile :: Maybe (Path Abs File),
    logLevelEcho :: Maybe Severity,
    logLevelStderr :: Maybe Severity,
    logLevelFile :: Maybe Severity
  }
  deriving stock (Eq, Show)

-- |Intermediate data type with optional fields for 'Ribosome.HostConfig'.
data CliConfig =
  CliConfig {
    log :: CliLogConfig
  }
  deriving stock (Eq, Show)
