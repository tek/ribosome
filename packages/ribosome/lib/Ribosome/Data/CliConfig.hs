module Ribosome.Data.CliConfig where
import Path (Path, Abs, File)
import Log (Severity)

data CliLogConfig =
  CliLogConfig {
    logFile :: Maybe (Path Abs File),
    logLevelEcho :: Maybe Severity,
    logLevelStderr :: Maybe Severity,
    logLevelFile :: Maybe Severity
  }
  deriving stock (Eq, Show)

data CliConfig =
  CliConfig {
    log :: CliLogConfig
  }
  deriving stock (Eq, Show)
