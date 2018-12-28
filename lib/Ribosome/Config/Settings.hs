module Ribosome.Config.Settings(
  persistenceDir,
) where

import Ribosome.Config.Setting (Setting(Setting))

persistenceDir :: Setting FilePath
persistenceDir = Setting "ribosome_persistence_dir" False Nothing
