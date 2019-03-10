module Ribosome.Error.Report.Class where

import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import System.Log.Logger (Priority(NOTICE, DEBUG))

class ReportError a where
  errorReport :: a -> ErrorReport

instance ReportError [Char] where
  errorReport msg = ErrorReport msg [msg] NOTICE

instance ReportError [[Char]] where
  errorReport (msg:extra) = ErrorReport msg (msg:extra) NOTICE
  errorReport [] = ErrorReport "empty error" ["empty error"] DEBUG