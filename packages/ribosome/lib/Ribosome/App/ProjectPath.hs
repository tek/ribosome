module Ribosome.App.ProjectPath where

import Path (Abs, Dir, Path, Rel, (</>))
import Path.IO (getCurrentDir)

import Ribosome.App.Error (RainbowError, ioError)

cwdProjectPath ::
  Members [Stop RainbowError, Embed IO] r =>
  Bool ->
  Path Rel Dir ->
  Sem r (Path Abs Dir)
cwdProjectPath append name = do
  cwd <- stopTryIOError err getCurrentDir
  pure (if append then cwd </> name else cwd)
  where
    err =
      ioError ["Could not determine current directory"]
