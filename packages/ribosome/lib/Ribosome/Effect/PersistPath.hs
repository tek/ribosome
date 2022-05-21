module Ribosome.Effect.PersistPath where

import Path (Abs, Dir, Path)

import Ribosome.Data.Setting (Setting (Setting))

data PersistPath :: Effect where
  PersistPath :: PersistPath m (Path Abs Dir)

makeSem ''PersistPath

setting :: Setting (Path Abs Dir)
setting =
  Setting "ribosome_persistence_dir" False Nothing
