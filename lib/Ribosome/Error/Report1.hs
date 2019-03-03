module Ribosome.Error.Report1(
  ErrorReport(..),
  ReportError(..),
  logErrorReport,
  reportErrorWith,
  reportError,
  reportErrorOr,
  reportErrorOr_,
  printAllErrors,
  runRiboReport,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader)
import Data.Foldable (traverse_)
import qualified Data.Map as Map (alter)
import Data.Text.Prettyprint.Doc (line, pretty, (<>))
import Data.Text.Prettyprint.Doc.Render.Terminal (putDoc)
import System.Log.Logger (logM)

import Ribosome.Api.Echo (echom)
import Ribosome.Control.Monad.Ribo (MonadRibo, Nvim, RiboE, runRiboE)
import qualified Ribosome.Control.Monad.Ribo as Ribo (getErrors, modifyErrors, pluginName)
import Ribosome.Control.Ribosome (Ribosome(..))
import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Data.Errors (ComponentName(ComponentName), Error(Error), Errors(Errors))
import Ribosome.Data.Time (epochSeconds)
import Ribosome.Error.Report.Class (ReportError(..))
import Ribosome.Nvim.Api.RpcCall (RpcError)

storeError' :: Int -> String -> ErrorReport -> Errors -> Errors
storeError' time name report (Errors errors) =
  Errors (Map.alter alter (ComponentName name) errors)
  where
    err = Error time report
    alter Nothing = Just [err]
    alter (Just current) = Just (err:current)

storeError :: (MonadRibo m, MonadIO m) => String -> ErrorReport -> m ()
storeError name e = do
  time <- epochSeconds
  Ribo.modifyErrors $ storeError' time name e

logErrorReport :: (MonadError RpcError m, MonadRibo m, Nvim m, MonadIO m) => ErrorReport -> m ()
logErrorReport (ErrorReport user logMsgs prio) = do
  name <- Ribo.pluginName
  liftIO $ traverse_ (logM name prio) logMsgs
  echom user

reportErrorWith :: (MonadError RpcError m, MonadRibo m, Nvim m, MonadIO m) => String -> (a -> ErrorReport) -> a -> m ()
reportErrorWith name cons err = do
  storeError name report
  logErrorReport report
  where
    report = cons err

reportError :: (MonadError RpcError m, MonadRibo m, Nvim m, MonadIO m, ReportError a) => String -> a -> m ()
reportError name =
  reportErrorWith name errorReport

reportErrorOr ::
  (MonadError RpcError m, MonadRibo m, Nvim m, MonadIO m, ReportError e) =>
  String ->
  (a -> m ()) ->
  Either e a ->
  m ()
reportErrorOr name =
  either $ reportError name

reportErrorOr_ ::
  (MonadError RpcError m, MonadRibo m, Nvim m, MonadIO m, ReportError e) =>
  String ->
  m () ->
  Either e a ->
  m ()
reportErrorOr_ name =
  reportErrorOr name . const

printAllErrors :: (MonadError RpcError m, MonadRibo m, Nvim m, MonadIO m) => m ()
printAllErrors = do
  errors <- Ribo.getErrors
  liftIO $ putDoc (pretty errors <> line)

runRiboReport ::
  (MonadError RpcError m, MonadReader (Ribosome s) m, MonadRibo m, Nvim m, MonadIO m, ReportError e) =>
  String ->
  RiboE s e m () ->
  m ()
runRiboReport componentName ma = do
  result <- runRiboE ma
  case result of
    Right _ -> return ()
    Left e -> reportError componentName e
