module Ribosome.Data.PersistPathError where

import Exon (exon)
import Path (Abs, Dir, Path)
import Polysemy.Log (Severity (Error))

import Ribosome.Host.Data.HandlerError (ErrorMessage (ErrorMessage), ToErrorMessage (toErrorMessage))
import Ribosome.Path (pathText)

data PersistPathError =
  Undefined
  |
  Permissions (Path Abs Dir)
  deriving stock (Eq, Show)

instance ToErrorMessage PersistPathError where
  toErrorMessage = \case
    Undefined ->
      ErrorMessage msg ["PersistPathError.Undefined"] Error
      where
        msg =
          "g:ribosome_persistence_dir unset and XDG not available."
    Permissions (pathText -> path) ->
      ErrorMessage msg ["PersistPathError.Permissions:", path] Error
      where
        msg =
          [exon|Couldn't create persistence dir '#{path}'|]
