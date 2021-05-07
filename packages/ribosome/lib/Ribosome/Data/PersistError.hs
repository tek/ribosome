module Ribosome.Data.PersistError where

import System.Log (Priority(INFO, NOTICE, ERROR))

import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Error.Report.Class (ReportError(..))

-- TODO use Path
data PersistError =
  FileNotReadable FilePath
  |
  NoSuchFile FilePath
  |
  Decode FilePath Text
  deriving (Eq, Show)

deepPrisms ''PersistError

instance ReportError PersistError where
  errorReport (FileNotReadable path) =
    ErrorReport msg ["PersistError.FileNotReadable:", toText path] NOTICE
    where
      msg = "persistence file not readable: " <> toText path
  errorReport (NoSuchFile path) =
    ErrorReport msg ["PersistError.SoSuchFile:", toText path] INFO
    where
      msg = "no persistence file present at " <> toText path
  errorReport (Decode path err) =
    ErrorReport msg ["PersistError.Decode:", toText path, err] ERROR
    where
      msg = "invalid data in persistence file, please delete it: " <> toText path
