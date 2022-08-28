module Ribosome.App.ProjectOptions where

import Ribosome.App.Data (
  Cachix (Cachix),
  CachixKey,
  CachixName,
  Github (Github),
  GithubOrg,
  GithubRepo (GithubRepo),
  Project (..),
  ProjectName (ProjectName),
  ProjectNames,
  SkipCachix (SkipCachix),
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

withCachixName ::
  Members [Stop RainbowError, Embed IO] r =>
  Maybe CachixKey ->
  CachixName ->
  Sem r Cachix
withCachixName key name =
  Cachix name <$> maybe (askRequired "Cachix public key?") pure key

askCachix ::
  Members [Stop RainbowError, Embed IO] r =>
  Maybe CachixKey ->
  Sem r (Maybe Cachix)
askCachix key =
  traverse (withCachixName key) =<< askUser "Cachix name? (Empty skips Cachix, ignore if unclear)"

cachixOption ::
  Members [Stop RainbowError, Embed IO] r =>
  Maybe CachixKey ->
  Maybe CachixName ->
  SkipCachix ->
  Sem r (Maybe Cachix)
cachixOption cachixKey cachixName = \case
  SkipCachix True ->
    pure Nothing
  SkipCachix False ->
    maybe (askCachix cachixKey) (fmap Just . withCachixName cachixKey) cachixName

projectOptions ::
  Members [Stop RainbowError, Embed IO] r =>
  Bool ->
  ProjectOptions ->
  Sem r Project
projectOptions appendNameToCwd opts = do
  names <- maybe resolveName pure (opts ^. #names)
  directory <- maybe (cwdProjectPath appendNameToCwd (names ^. #nameDir)) pure (opts ^. #directory)
  let name = names ^. #name
  github <- maybe (askGithub name repo) (fmap Just . withOrg name repo) (opts ^. #githubOrg)
  cachix <- cachixOption (opts ^. #cachixKey) (opts ^. #cachixName) (opts ^. #skipCachix)
  pure Project {..}
  where
    repo =
      opts ^. #githubRepo
    branch =
      fromMaybe "master" (opts ^. #branch)
