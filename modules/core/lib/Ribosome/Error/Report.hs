module Ribosome.Error.Report where

import Control.Monad ((<=<))
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Foldable (traverse_)
import Data.Functor (void)
import qualified Data.Map.Strict as Map (alter)
import Data.Text.Prettyprint.Doc (line, pretty, (<>))
import Data.Text.Prettyprint.Doc.Render.Terminal (putDoc)
import System.Log (Priority(NOTICE))

import Ribosome.Api.Echo (echom)
import Ribosome.Control.Monad.Ribo (MonadRibo, Nvim, NvimE, RNeovim, Ribo, runRibo)
import qualified Ribosome.Control.Monad.Ribo as Ribo (getErrors, modifyErrors, pluginName)
import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Data.Errors (ComponentName(ComponentName), Error(Error), Errors(Errors))
import Ribosome.Error.Report.Class (ReportError(..))
import Ribosome.Log (logAs)
import Ribosome.Nvim.Api.RpcCall (RpcError)
import Ribosome.System.Time (epochSeconds)

storeError' :: Int -> Text -> ErrorReport -> Errors -> Errors
storeError' time name report (Errors errors) =
  Errors (Map.alter alter (ComponentName name) errors)
  where
    err = Error time report
    alter Nothing = Just [err]
    alter (Just current) = Just (err:current)

storeError :: (MonadRibo m, MonadIO m) => Text -> ErrorReport -> m ()
storeError name e = do
  time <- epochSeconds
  Ribo.modifyErrors $ storeError' time name e

logErrorReport ::
  (MonadRibo m, NvimE e m, MonadIO m) =>
  ErrorReport ->
  m ()
logErrorReport (ErrorReport user logMsgs prio) = do
  name <- Ribo.pluginName
  liftIO $ traverse_ (logAs prio name) logMsgs
  when (prio >= NOTICE) (echom user)

processErrorReport ::
  (MonadRibo m, NvimE e m, MonadIO m) =>
  Text ->
  ErrorReport ->
  m ()
processErrorReport name report = do
  storeError name report
  logErrorReport report

processErrorReport' ::
  (MonadRibo m, Nvim m, MonadIO m) =>
  Text ->
  ErrorReport ->
  m ()
processErrorReport' name =
  void . runExceptT @RpcError . processErrorReport name

reportErrorWith ::
  (MonadRibo m, NvimE e m, MonadIO m) =>
  Text ->
  (a -> ErrorReport) ->
  a ->
  m ()
reportErrorWith name cons err =
  processErrorReport name (cons err)

reportError ::
  MonadRibo m =>
  NvimE e m =>
  MonadIO m =>
  ReportError a =>
  Text ->
  a ->
  m ()
reportError name =
  reportErrorWith name errorReport

reportErrorOr ::
  (MonadRibo m, NvimE e m, MonadIO m, ReportError e) =>
  Text ->
  (a -> m ()) ->
  Either e a ->
  m ()
reportErrorOr name =
  either $ reportError name

reportErrorOr_ ::
  (MonadError RpcError m, MonadRibo m, NvimE e m, MonadIO m, ReportError e) =>
  Text ->
  m () ->
  Either e a ->
  m ()
reportErrorOr_ name =
  reportErrorOr name . const

reportError' ::
  ∀ e m a .
  (MonadRibo m, Nvim m, MonadIO m, ReportError e) =>
  Text ->
  Either e a ->
  m ()
reportError' _ (Right _) =
  return ()
reportError' componentName (Left e) =
  void $ runExceptT @RpcError $ reportError componentName e

printAllErrors :: (MonadRibo m, NvimE e m, MonadIO m) => m ()
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
