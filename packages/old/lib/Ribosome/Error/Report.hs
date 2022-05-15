module Ribosome.Error.Report where

import qualified Data.Map.Strict as Map (alter)
import Prettyprinter (line, pretty)
import Prettyprinter.Render.Terminal (putDoc)
import System.Log (Priority(NOTICE))

import Ribosome.Api.Echo (echom)
import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Data.Errors (ComponentName(ComponentName), Error(Error), Errors(Errors))
import Ribosome.Error.Report.Class (ReportError(..))
import Ribosome.System.Time (epochSeconds)

storeError' :: Int -> Text -> ErrorReport -> Errors -> Errors
storeError' time name report (Errors errors) =
  Errors (Map.alter alter (ComponentName name) errors)
  where
    err = Error time report
    alter Nothing = Just [err]
    alter (Just current) = Just (err:current)

storeError name e = do
  time <- epochSeconds
  Ribo.modifyErrors $ storeError' time name e

logErrorReport ::
  ErrorReport ->
  m ()
logErrorReport (ErrorReport user logMsgs prio) = do
  name <- Ribo.pluginName
  liftIO $ traverse_ (logAs prio name) logMsgs
  when (prio >= NOTICE) (echom user)

processErrorReport ::
  Text ->
  ErrorReport ->
  m ()
processErrorReport name report = do
  storeError name report
  logErrorReport report

processErrorReport' ::
  Text ->
  ErrorReport ->
  m ()
processErrorReport' name =
  void . runExceptT @RpcError . processErrorReport name

reportErrorWith ::
  Text ->
  (a -> ErrorReport) ->
  a ->
  m ()
reportErrorWith name cons err =
  processErrorReport name (cons err)

reportError ::
  Member Rpc r =>
  ReportError a =>
  Text ->
  a ->
  m ()
reportError name =
  reportErrorWith name errorReport

reportErrorOr ::
  Text ->
  (a -> m ()) ->
  Either e a ->
  m ()
reportErrorOr name =
  either $ reportError name

reportErrorOr_ ::
  Text ->
  m () ->
  Either e a ->
  m ()
reportErrorOr_ name =
  reportErrorOr name . const

reportError' ::
  ∀ e m a .
  Text ->
  Either e a ->
  m ()
reportError' _ (Right _) =
  pure ()
reportError' componentName (Left e) =
  void $ runExceptT @RpcError $ reportError componentName e

printAllErrors = do
  errors <- Ribo.getErrors
  liftIO $ putDoc (pretty errors <> line)

runRiboReport ::
  ∀ e s.
  ReportError e =>
  Text ->
  Ribo s e () ->
  RNeovim s ()
runRiboReport componentName =
  reportError' componentName <=< runRibo
