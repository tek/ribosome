-- |Provide paths for 'Ribosome.Persist'
module Ribosome.Effect.PersistPath where

import Path (Abs, Dir, Path, Rel)

import Ribosome.Data.Setting (Setting (Setting))

-- |This is a utility effect for 'Persist', determining the root directory for persistence files.
data PersistPath :: Effect where
  -- |Return the root if 'Nothing' is given, or the subdir of the root if 'Just' is given.
  PersistPath :: Maybe (Path Rel Dir) -> PersistPath m (Path Abs Dir)

makeSem ''PersistPath

-- |This setting may be used to specify the root directory for all plugins.
-- The default is to use the XDG cache dir.
setting :: Setting (Path Abs Dir)
setting =
  Setting "ribosome_persistence_dir" False Nothing

-- |Get the root directory for persistence files.
persistRoot ::
  Member PersistPath r =>
  Sem r (Path Abs Dir)
persistRoot =
  persistPath Nothing

-- |Get a subdir of the root directory for persistence files.
persistSubPath ::
  Member PersistPath r =>
  Path Rel Dir ->
  Sem r (Path Abs Dir)
persistSubPath p =
  persistPath (Just p)
