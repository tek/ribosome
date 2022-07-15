module Ribosome.App.ProjectOptions where

import Ribosome.App.Data (
  Github (Github),
  GithubOrg,
  GithubRepo (GithubRepo),
  Project (..),
  ProjectName (ProjectName),
  ProjectNames,
  )
import Ribosome.App.Error (RainbowError, appError)
import Ribosome.App.Options (ProjectOptions)
import qualified Ribosome.App.ProjectNames as ProjectNames
import Ribosome.App.ProjectPath (cwdProjectPath)
import Ribosome.App.UserInput (askRequired, askUser)

resolveName ::
  Members [Stop RainbowError, Embed IO] r =>
  Sem r ProjectNames
resolveName = do
  name <- askRequired "Name of the project?"
  stopEitherWith err (ProjectNames.parse name)
  where
    err =
      appError . pure

askGithubRepo ::
  Members [Stop RainbowError, Embed IO] r =>
  ProjectName ->
  Sem r GithubRepo
askGithubRepo (ProjectName name) =
  fromMaybe (GithubRepo name) <$> askUser "Github repository name? (Empty uses project name)"

withOrg ::
  Members [Stop RainbowError, Embed IO] r =>
  ProjectName ->
  Maybe GithubRepo ->
  GithubOrg ->
  Sem r Github
withOrg name repo org =
  Github org <$> maybe (askGithubRepo name) pure repo

askGithub ::
  Members [Stop RainbowError, Embed IO] r =>
  ProjectName ->
  Maybe GithubRepo ->
  Sem r (Maybe Github)
askGithub name repo =
  traverse (withOrg name repo) =<< askUser "Github organization? (Empty skips Github)"

projectOptions ::
  Members [Stop RainbowError, Embed IO] r =>
  ProjectOptions ->
  Sem r Project
projectOptions opts = do
  names <- maybe resolveName pure (opts ^. #names)
  directory <- maybe (cwdProjectPath (names ^. #nameDir)) pure (opts ^. #directory)
  let name = names ^. #name
  github <- maybe (askGithub name repo) (fmap Just . withOrg name repo) (opts ^. #githubOrg)
  pure Project {..}
  where
    repo =
      opts ^. #githubRepo
    branch =
      fromMaybe "master" (opts ^. #branch)
