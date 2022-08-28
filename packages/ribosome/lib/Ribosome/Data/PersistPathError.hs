-- |Error for 'Ribosome.PersistPath'.
module Ribosome.Data.PersistPathError where

import Exon (exon)
import Path (Abs, Dir, Path)
import Polysemy.Log (Severity (Error))

import Ribosome.Host.Data.Report (Report (Report), Reportable (toReport))
import Ribosome.Path (pathText)

-- |The errors emitted by the effect 'Ribosome.PersistPath'.
data PersistPathError =
  -- |Cannot determine the cache directory.
  Undefined
  |
  -- |General permissions error.
  Permissions (Path Abs Dir)
  deriving stock (Eq, Show)

instance Reportable PersistPathError where
  toReport = \case
    Undefined ->
      Report msg ["PersistPathError.Undefined"] Error
      where
        msg =
          "g:ribosome_persistence_dir unset and XDG not available."
    Permissions (pathText -> path) ->
      Report msg ["PersistPathError.Permissions:", path] Error
      where
        msg =
          [exon|Couldn't create persistence dir '#{path}'|]
