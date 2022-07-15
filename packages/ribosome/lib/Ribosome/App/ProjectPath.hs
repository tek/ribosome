module Ribosome.App.ProjectPath where

import Path (Abs, Dir, Path, Rel, (</>))
import Path.IO (getCurrentDir)

import Ribosome.App.Error (RainbowError, ioError)

cwdProjectPath ::
  Members [Stop RainbowError, Embed IO] r =>
  Path Rel Dir ->
  Sem r (Path Abs Dir)
cwdProjectPath name = do
  cwd <- stopTryIOError err getCurrentDir
  pure (cwd </> name)
  where
    err =
      ioError ["Could not determine current directory"]
