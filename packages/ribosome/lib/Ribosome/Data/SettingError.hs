module Ribosome.Data.SettingError where

import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)
import System.Log (Priority(NOTICE))

import Ribosome.Data.ErrorReport (ErrorReport(..))
import Ribosome.Error.Report.Class (ReportError(..))

data SettingError =
  Decode Text (Doc AnsiStyle)
  |
  Unset Text
  deriving stock (Show)

deepPrisms ''SettingError

instance ReportError SettingError where
  errorReport (Decode name message) =
    ErrorReport ("invalid setting: " <> name) ["failed to decode setting `" <> name <> "`", show message] NOTICE
  errorReport (Unset name) =
    ErrorReport ("required setting unset: " <> name) ["unset setting: `" <> name <> "`"] NOTICE
