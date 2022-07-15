module Ribosome.App.ProjectNames where

import Path (parseRelDir)

import Ribosome.App.Data (ModuleName (ModuleName), ProjectName (ProjectName), ProjectNames (..))
import Ribosome.Host.Text (pascalCase)

parse ::
  IsString err =>
  String ->
  Either err ProjectNames
parse raw = do
  let
    name = ProjectName (toText raw)
    modRaw = pascalCase raw
    moduleName = ModuleName (toText modRaw)
  nameDir <- maybe err pure (parseRelDir raw)
  moduleNameDir <- maybe err pure (parseRelDir modRaw)
  pure ProjectNames {..}
  where
    err =
      Left "The project name must be usable as a directory name"
