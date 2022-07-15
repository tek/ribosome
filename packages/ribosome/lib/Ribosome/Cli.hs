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
import Ribosome.Data.PluginConfig (PluginConfig (PluginConfig, name))
import Ribosome.Data.PluginName (PluginName (PluginName))
import Ribosome.Host.Data.HostConfig (HostConfig (HostConfig), LogConfig (LogConfig))

logParser ::
  Path Abs Dir ->
  Parser CliLogConfig
logParser cwd = do
  logFile <- optional (option (filePathOption cwd) (long "log-file"))
  levelEcho <- optional (option severityOption (long "log-level-echo"))
  levelStderr <- optional (option severityOption (long "log-level-stderr"))
  levelFile <- optional (option severityOption (long "log-level-file"))
  pure (CliLogConfig logFile levelEcho levelStderr levelFile)

confParser ::
  Path Abs Dir ->
  Parser CliConfig
confParser cwd =
  CliConfig <$> logParser cwd

parseCli ::
  PluginName ->
  IO CliConfig
parseCli (PluginName name) = do
  cwd <- getCurrentDir
  customExecParser parserPrefs (info (confParser cwd <**> helper) desc)
  where
    parserPrefs =
      prefs (showHelpOnEmpty <> showHelpOnError)
    desc =
      fullDesc <> header [exon|#{toString name} is a Neovim plugin.|]

withDefault :: PluginConfig -> CliConfig -> PluginConfig
withDefault (PluginConfig name (HostConfig defLog)) cliConfig =
  PluginConfig name (HostConfig log)
  where
    CliConfig (CliLogConfig file levelEcho levelStderr levelFile) =
      cliConfig
    LogConfig defFile defLevelEcho defLevelStderr defLevelFile conc =
      defLog
    log =
      LogConfig (file <|> defFile) (fromMaybe defLevelEcho levelEcho) (fromMaybe defLevelStderr levelStderr)
      (fromMaybe defLevelFile levelFile) conc

withCli ::
  PluginConfig ->
  (PluginConfig -> IO a) ->
  IO a
withCli defaultConf@PluginConfig {name} f = do
  cliConfig <- parseCli name
  f (withDefault defaultConf cliConfig)
