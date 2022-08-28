-- |@optparse-applicative@ parsers for the Ribosome CLI.
module Ribosome.Cli where

import Exon (exon)
import Options.Applicative (
  Parser,
  customExecParser,
  fullDesc,
  header,
  helper,
  info,
  long,
  option,
  prefs,
  showHelpOnEmpty,
  showHelpOnError,
  )
import Path (Abs, Dir, Path)
import Path.IO (getCurrentDir)

import Ribosome.CliParser (filePathOption, severityOption)
import Ribosome.Data.CliConfig (CliConfig (CliConfig), CliLogConfig (CliLogConfig))
import Ribosome.Data.PluginName (PluginName (PluginName))
import Ribosome.Host.Data.HostConfig (HostConfig (HostConfig), LogConfig (LogConfig))

-- |Parse the options related to logging.
logParser ::
  Path Abs Dir ->
  Parser CliLogConfig
logParser cwd = do
  logFile <- optional (option (filePathOption cwd) (long "log-file"))
  levelEcho <- optional (option severityOption (long "log-level-echo"))
  levelStderr <- optional (option severityOption (long "log-level-stderr"))
  levelFile <- optional (option severityOption (long "log-level-file"))
  pure (CliLogConfig logFile levelEcho levelStderr levelFile)

-- |Parse the host config as well as the arbitrary user defined config.
confParser ::
  Path Abs Dir ->
  Parser c ->
  Parser (CliConfig, c)
confParser cwd customParser = do
  cli <- CliConfig <$> logParser cwd
  custom <- customParser
  pure (cli, custom)

-- |Parse the host config as well as the arbitrary user defined config, in 'IO'.
parseCli ::
  PluginName ->
  Parser c ->
  IO (CliConfig, c)
parseCli (PluginName name) customParser = do
  cwd <- getCurrentDir
  customExecParser parserPrefs (info (helper <*> confParser cwd customParser) desc)
  where
    parserPrefs =
      prefs (showHelpOnEmpty <> showHelpOnError)
    desc =
      fullDesc <> header [exon|#{toString name} is a Neovim plugin.|]

-- |Parse the CLI options for a plugin config and update a default 'HostConfig' with the CLI options.
withDefault :: HostConfig -> CliConfig -> HostConfig
withDefault (HostConfig defLog) cliConfig =
  HostConfig log
  where
    CliConfig (CliLogConfig file levelEcho levelStderr levelFile) =
      cliConfig
    LogConfig defFile defLevelEcho defLevelStderr defLevelFile conc =
      defLog
    log =
      LogConfig (file <|> defFile) (fromMaybe defLevelEcho levelEcho) (fromMaybe defLevelStderr levelStderr)
      (fromMaybe defLevelFile levelFile) conc

-- |Parse the CLI options for a plugin config and pass an updated default 'HostConfig' to a callback.
withCli ::
  PluginName ->
  HostConfig ->
  Parser c ->
  (HostConfig -> c -> IO a) ->
  IO a
withCli name defaultConf customParser f = do
  (cliConfig, custom) <- parseCli name customParser
  f (withDefault defaultConf cliConfig) custom
