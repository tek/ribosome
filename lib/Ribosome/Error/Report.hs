module Ribosome.Error.Report(
  ErrorReport(..),
  ReportError(..),
  logErrorReport,
  reportErrorWith,
  reportError,
  reportErrorOr,
  reportErrorOr_,
  printAllErrors,
) where

import qualified Control.Lens as Lens (over)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import qualified Data.Map as Map (alter, toList)
import Data.Text.Prettyprint.Doc (Doc, pretty, line, vsep, (<+>), (<>))
import Data.Text.Prettyprint.Doc.Render.Terminal (putDoc, AnsiStyle)
import System.Log.Logger (logM, Priority(NOTICE, DEBUG))

import Ribosome.Api.Echo (echom)
import Ribosome.Control.Monad.Ribo (Ribo)
import qualified Ribosome.Control.Ribo as Ribo (modify, name, modifyErrors, getErrors)
import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Data.Errors (Errors(Errors), ComponentName(ComponentName), Error(Error))
import Ribosome.Data.Time (epochSeconds)

class ReportError a where
  errorReport :: a -> ErrorReport

instance ReportError [Char] where
  errorReport msg = ErrorReport msg [msg] NOTICE

instance ReportError [[Char]] where
  errorReport (msg:extra) = ErrorReport msg (msg:extra) NOTICE
  errorReport [] = ErrorReport "empty error" ["empty error"] DEBUG

storeError' :: Int -> String -> ErrorReport -> Errors -> Errors
storeError' time name report (Errors errors) =
  Errors (Map.alter alter (ComponentName name) errors)
  where
    err = Error time report
    alter Nothing = Just [err]
    alter (Just current) = Just (err:current)

storeError :: String -> ErrorReport -> Ribo d ()
storeError name e = do
  time <- epochSeconds
  Ribo.modifyErrors $ storeError' time name e

logErrorReport :: ErrorReport -> Ribo d ()
logErrorReport (ErrorReport user logMsgs prio) = do
  name <- Ribo.name
  liftIO $ traverse_ (logM name prio) logMsgs
  echom user

reportErrorWith :: String -> (a -> ErrorReport) -> a -> Ribo d ()
reportErrorWith name cons err = do
  storeError name report
  logErrorReport report
  where
    report = cons err

reportError :: ReportError a => String -> a -> Ribo d ()
reportError name =
  reportErrorWith name errorReport

reportErrorOr :: ReportError e => String -> (a -> Ribo d ()) -> Either e a -> Ribo d ()
reportErrorOr name =
  either $ reportError name

reportErrorOr_ :: ReportError e => String -> Ribo d () -> Either e a -> Ribo d ()
reportErrorOr_ name =
  reportErrorOr name . const

printAllErrors :: Ribo e ()
printAllErrors = do
  errors <- Ribo.getErrors
  liftIO $ putDoc $ pretty errors
