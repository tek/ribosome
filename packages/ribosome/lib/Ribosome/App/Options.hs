module Ribosome.App.Options where

import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  ReadM,
  argument,
  command,
  customExecParser,
  fullDesc,
  header,
  help,
  helper,
  hsubparser,
  info,
  long,
  metavar,
  option,
  prefs,
  progDesc,
  readerError,
  short,
  showHelpOnEmpty,
  showHelpOnError,
  str,
  switch,
  )
import Options.Applicative.Types (readerAsk)
import Path (Abs, Dir, Path)
import Path.IO (getCurrentDir)
import Prelude hiding (Mod)

import Ribosome.App.Data (
  Author,
  Branch,
  CachixKey,
  CachixName,
  FlakeUrl,
  GithubOrg,
  GithubRepo,
  Maintainer,
  PrintDir (PrintDir),
  ProjectNames (..),
  SkipCachix (SkipCachix),
  )
import qualified Ribosome.App.ProjectNames as ProjectNames
import Ribosome.CliParser (dirPathOption)

data ProjectOptions =
  ProjectOptions {
    names :: Maybe ProjectNames,
    directory :: Maybe (Path Abs Dir),
    branch :: Maybe Branch,
    githubOrg :: Maybe GithubOrg,
    githubRepo :: Maybe GithubRepo,
    skipCachix :: SkipCachix,
    cachixName :: Maybe CachixName,
    cachixKey :: Maybe CachixKey
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)

data NewOptions =
  NewOptions {
    project :: ProjectOptions,
    flakeUrl :: Maybe FlakeUrl,
    printDir :: PrintDir,
    author :: Maybe Author,
    maintainer :: Maybe Maintainer
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)

data Command =
  New NewOptions
  |
  Boot ProjectOptions
  deriving stock (Eq, Show)

data GlobalOptions =
  GlobalOptions {
    quiet :: Maybe Bool,
    force :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)

data Options =
  Options {
    global :: GlobalOptions,
    cmd :: Command
  }
  deriving stock (Eq, Show)

projectNamesOption ::
  ReadM ProjectNames
projectNamesOption = do
  raw <- readerAsk
  either readerError pure (ProjectNames.parse raw)

directoryParser ::
  Path Abs Dir ->
  Parser (Maybe (Path Abs Dir))
directoryParser cwd =
  optional (option (dirPathOption cwd) (long "directory" <> short 'd' <> help dirHelp))
  where
    dirHelp =
      "The directory for the new project. Defaults to the project name as subdir of the current dir"

projectParser ::
  Path Abs Dir ->
  Parser ProjectOptions
projectParser cwd =
  ProjectOptions
  <$>
  optional (argument projectNamesOption (metavar "NAME" <> help "Name of the project"))
  <*>
  directoryParser cwd
  <*>
  optional (option str (long "branch" <> short 'b' <> help branchHelp))
  <*>
  optional (option str (long "github-org" <> short 'o' <> help orgHelp))
  <*>
  optional (option str (long "github-repo" <> short 'r' <> help repoHelp))
  <*>
  (SkipCachix <$> switch (long "skip-cachix" <> help "Don't ask for cachix credentials"))
  <*>
  optional (option str (long "cachix" <> short 'c' <> help cachixHelp))
  <*>
  optional (option str (long "cachix-key" <> short 'k' <> help cachixKeyHelp))
  where
    orgHelp =
      "Name of the Github org, for generating vim boot files that download binaries built by Actions"
    repoHelp =
      "Name of the Github repo, in case it differs from the project name"
    branchHelp =
      "Main branch for creating binaries via Github Actions, defaults to 'master'"
    cachixHelp =
      "Name of the cachix cache to push to from Github Actions, and pull from in the Neovim boot file"
    cachixKeyHelp =
      "The public key for the cachix cache, found at https://app.cachix.org/cache/<name>"

newParser ::
  Path Abs Dir ->
  Parser NewOptions
newParser cwd =
  NewOptions
  <$>
  projectParser cwd
  <*>
  optional (option str (long "flake-url" <> short 'f' <> help "Custom URL for the Ribosome flake"))
  <*>
  (PrintDir <$> switch (long "print-dir" <> short 'p' <> help "Write the generated directory to stdout"))
  <*>
  optional (option str (long "author" <> short 'a' <> help "Author for the Cabal file"))
  <*>
  optional (option str (long "maintainer" <> short 'm' <> help "Maintainer for the Cabal file"))

newCommand ::
  Path Abs Dir ->
  Mod CommandFields Command
newCommand cwd =
  command "new" (New <$> info (newParser cwd) (progDesc "Generate a new project for a Neovim plugin"))

bootCommand ::
  Path Abs Dir ->
  Mod CommandFields Command
bootCommand cwd =
  command "boot" (Boot <$> info (projectParser cwd) (progDesc "Generate the Neovim boot file"))

globalParser :: Parser GlobalOptions
globalParser = do
  quiet <- optional (switch (long "quiet" <> short 'q' <> help "Suppress informational messages"))
  force <- optional (switch (long "force" <> short 'f' <> help "Overwrite existing files"))
  pure (GlobalOptions {..})

appParser ::
  Path Abs Dir ->
  Parser Options
appParser cwd =
  Options <$> globalParser <*> hsubparser (mconcat [newCommand cwd, bootCommand cwd])

parseCli ::
  IO Options
parseCli = do
  cwd <- getCurrentDir
  customExecParser parserPrefs (info (appParser cwd <**> helper) desc)
  where
    parserPrefs =
      prefs (showHelpOnEmpty <> showHelpOnError)
    desc =
      fullDesc <> header "Tools for maintaining Ribosome plugins"
