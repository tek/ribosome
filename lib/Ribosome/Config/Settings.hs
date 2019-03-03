module Ribosome.Config.Settings(
  persistenceDir,
) where

import Ribosome.Data.Setting (Setting(Setting))

persistenceDir :: Setting FilePath
persistenceDir = Setting "ribosome_persistence_dir" False Nothing
