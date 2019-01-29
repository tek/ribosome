module Ribosome.Error.Report(
  ErrorReport(..),
  ReportError(..),
  logErrorReport,
  reportErrorWith,
  reportError,
  reportErrorOr,
  reportErrorOr_,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import System.Log (Priority)
import System.Log.Logger (logM)
import Ribosome.Api.Echo (echom)
import Ribosome.Control.Monad.Ribo (Ribo)
import qualified Ribosome.Control.Ribo as Ribo (name)

data ErrorReport =
  ErrorReport {
    errorReportUser :: String,
    errorReportLog :: [String],
    errorReportPriority :: Priority
  }
  deriving (Eq, Show)

class ReportError a where
  errorReport :: a -> ErrorReport

logErrorReport :: ErrorReport -> Ribo d ()
logErrorReport (ErrorReport user logMsgs prio) = do
  name <- Ribo.name
  liftIO $ traverse_ (logM name prio) logMsgs
  echom user

reportErrorWith :: (a -> ErrorReport) -> a -> Ribo d ()
reportErrorWith cons =
  logErrorReport . cons

reportError :: ReportError a => a -> Ribo d ()
reportError =
  reportErrorWith errorReport

reportErrorOr :: ReportError e => (a -> Ribo d ()) -> Either e a -> Ribo d ()
reportErrorOr = either reportError

reportErrorOr_ :: ReportError e => Ribo d () -> Either e a -> Ribo d ()
reportErrorOr_ = reportErrorOr . const
