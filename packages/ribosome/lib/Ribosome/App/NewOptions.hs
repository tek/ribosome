module Ribosome.App.NewOptions where

import Path (Dir, Path, Rel, parseRelDir)
import Rainbow (chunk, fore, magenta)

import qualified Ribosome.App.Data as Data
import Ribosome.App.Data (FlakeUrl, NewProject (NewProject))
import Ribosome.App.Error (RainbowError, appError)
import Ribosome.App.Options (NewOptions)
import Ribosome.App.ProjectOptions (projectOptions)
import Ribosome.App.UserInput (askRequired)

defaultFlakeUrl :: FlakeUrl
defaultFlakeUrl =
  "git+https://git.tryp.io/tek/ribosome?ref=polysemy"

parseDir ::
  ToText a =>
  ToString a =>
  Member (Stop RainbowError) r =>
  a ->
  Sem r (Path Rel Dir)
parseDir name =
  stopNote invalidName (parseRelDir (toString name))
  where
    invalidName =
      appError [fore magenta (chunk (toText name)), " cannot be used as a directory name."]

newOptions ::
  Members [Stop RainbowError, Embed IO] r =>
  NewOptions ->
  Sem r NewProject
newOptions opts = do
  project <- projectOptions (opts ^. #project)
  author <- maybe (askRequired "Author name for Cabal file?") pure (opts ^. #author)
  maintainer <- maybe (askRequired "Maintainer email for Cabal file?") pure (opts ^. #maintainer)
  let
    flakeUrl =
      fromMaybe defaultFlakeUrl (opts ^. #flakeUrl)
    printDir =
      opts ^. #printDir
  pure NewProject {..}
