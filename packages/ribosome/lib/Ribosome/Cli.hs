module Ribosome.Cli where

import Exon (exon)
import Log (Severity, parseSeverity)
import Options.Applicative (Parser, ReadM, option, readerError, customExecParser, info, helper, prefs, showHelpOnEmpty, showHelpOnError, fullDesc, header, long)
import Options.Applicative.Types (readerAsk)
import Path (Abs, Dir, File, Path, SomeBase (Abs, Rel), parseSomeFile, (</>))
import Path.IO (getCurrentDir)

import Ribosome.Data.CliConfig (CliConfig (CliConfig), CliLogConfig (CliLogConfig))
import Ribosome.Data.PluginConfig (PluginConfig (PluginConfig, name))
import Ribosome.Data.PluginName (PluginName (PluginName))
import Ribosome.Host.Data.HostConfig (LogConfig(LogConfig), HostConfig (HostConfig))

somePath ::
  Path Abs Dir ->
  SomeBase t ->
  Path Abs t
somePath cwd = \case
  Abs p ->
    p
  Rel p ->
    cwd </> p

severityOption :: ReadM Severity
severityOption = do
  raw <- readerAsk
  maybe (readerError [exon|invalid log level: #{raw}|]) pure (parseSeverity (toText raw))

filePathOption ::
  Path Abs Dir ->
  ReadM (Path Abs File)
filePathOption cwd = do
  raw <- readerAsk
  either (const (readerError [exon|not a valid file path: #{show raw}|])) (pure . somePath cwd) (parseSomeFile raw)

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
    LogConfig defFile defLevelEcho defLevelStderr defLevelFile =
      defLog
    log =
      LogConfig (file <|> defFile) (fromMaybe defLevelEcho levelEcho) (fromMaybe defLevelStderr levelStderr)
      (fromMaybe defLevelFile levelFile)

withCli ::
  PluginConfig ->
  (PluginConfig -> IO a) ->
  IO a
withCli defaultConf@PluginConfig {name} f = do
  cliConfig <- parseCli name
  f (withDefault defaultConf cliConfig)
