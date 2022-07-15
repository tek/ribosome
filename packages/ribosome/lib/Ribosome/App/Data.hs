module Ribosome.App.Data where

import Path (Abs, Dir, Path, Rel)

newtype ProjectName =
  ProjectName { unProjectName :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

newtype ModuleName =
  ModuleName { unModuleName :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

data ProjectNames =
  ProjectNames {
    name :: ProjectName,
    nameDir :: Path Rel Dir,
    moduleName :: ModuleName,
    moduleNameDir :: Path Rel Dir
  }
  deriving stock (Eq, Show, Generic)

newtype GithubOrg =
  GithubOrg { unGithubOrg :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

newtype GithubRepo =
  GithubRepo { unGithubRepo :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

newtype FlakeUrl =
  FlakeUrl { unFlakeUrl :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

newtype Branch =
  Branch { unBranch :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

newtype Author =
  Author { unAuthor :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

newtype Maintainer =
  Maintainer { unMaintainer :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

newtype Year =
  Year { unYear :: Int }
  deriving stock (Eq, Show)
  deriving newtype (Num, Real, Enum, Integral, Ord)

newtype PrintDir =
  PrintDir { unPrintDir :: Bool }
  deriving stock (Eq, Show)

instance Default PrintDir where
  def =
    PrintDir False

data Github =
  Github {
    org :: GithubOrg,
    repo :: GithubRepo
  }
  deriving stock (Eq, Show, Generic)

data Project =
  Project {
    names :: ProjectNames,
    github :: Maybe Github,
    directory :: Path Abs Dir,
    branch :: Branch
  }
  deriving stock (Eq, Show, Generic)

data Global =
  Global {
    quiet :: Bool,
    force :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance Default Global where
  def =
    Global False False

data NewProject =
  NewProject {
    project :: Project,
    flakeUrl :: FlakeUrl,
    printDir :: PrintDir,
    author :: Author,
    maintainer :: Maintainer
  }
  deriving stock (Eq, Show, Generic)
