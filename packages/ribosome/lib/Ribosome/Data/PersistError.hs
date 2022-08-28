-- |Error for 'Ribosome.Persist'.
module Ribosome.Data.PersistError where

import Exon (exon)
import Polysemy.Log (Severity (Error))

import Ribosome.Data.PersistPathError (PersistPathError)
import Ribosome.Host.Data.Report (Report (Report), Reportable (toReport))

-- |The errors emitted by the effect 'Ribosome.PersistPath'.
data PersistError =
  -- |Can't access the persistence files.
  Permission Text
  |
  -- |Data in the persistence file has invalid format.
  Decode Text Text
  |
  -- |'Ribosome.PeristPath' threw an error.
  Path PersistPathError
  deriving stock (Eq, Show)

instance Reportable PersistError where
  toReport = \case
    Permission path ->
      Report msg ["PersistError.Permission:", path] Error
      where
        msg =
          [exon|Insufficient permissions for persistence file: #{path}|]
    Decode path err ->
      Report msg ["PersistError.Decode:", path, err] Error
      where
        msg =
          [exon|invalid data in persistence file, please delete it: #{path}|]
    Path err ->
      toReport err
