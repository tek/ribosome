module Ribosome.Effect.PersistPath where

import Path (Abs, Dir, Path, Rel)

import Ribosome.Data.Setting (Setting (Setting))

data PersistPath :: Effect where
  PersistPath :: Maybe (Path Rel Dir) -> PersistPath m (Path Abs Dir)

makeSem ''PersistPath

setting :: Setting (Path Abs Dir)
setting =
  Setting "ribosome_persistence_dir" False Nothing

persistRoot ::
  Member PersistPath r =>
  Sem r (Path Abs Dir)
persistRoot =
  persistPath Nothing

persistSubPath ::
  Member PersistPath r =>
  Path Rel Dir ->
  Sem r (Path Abs Dir)
persistSubPath p =
  persistPath (Just p)
