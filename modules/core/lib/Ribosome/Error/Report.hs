module Ribosome.Error.Report where

import Control.Monad ((<=<))
import Control.Monad.DeepError (MonadDeepError)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Except (runExceptT)
import Data.Foldable (traverse_)
import Data.Functor (void)
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
import Ribosome.Error.Report.Class (ReportError(..))
import Ribosome.Nvim.Api.RpcCall (RpcError)
import Ribosome.System.Time (epochSeconds)

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

logErrorReport ::
  (MonadDeepError e RpcError m, MonadRibo m, Nvim m, MonadIO m) =>
  ErrorReport ->
  m ()
logErrorReport (ErrorReport user logMsgs prio) = do
  name <- Ribo.pluginName
  liftIO $ traverse_ (logM name prio) logMsgs
  echom user

processErrorReport ::
  (MonadDeepError e RpcError m, MonadRibo m, Nvim m, MonadIO m) =>
  String ->
  ErrorReport ->
  m ()
processErrorReport name report = do
  storeError name report
  logErrorReport report

processErrorReport' ::
  (MonadRibo m, Nvim m, MonadIO m) =>
  String ->
  ErrorReport ->
  m ()
processErrorReport' name =
  void . runExceptT @RpcError . processErrorReport name

reportErrorWith ::
  (MonadDeepError e RpcError m, MonadRibo m, Nvim m, MonadIO m) =>
  String ->
  (a -> ErrorReport) ->
  a ->
  m ()
reportErrorWith name cons err =
  processErrorReport name (cons err)

reportError :: (MonadDeepError e RpcError m, MonadRibo m, Nvim m, MonadIO m, ReportError a) => String -> a -> m ()
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

reportError' ::
  (MonadRibo m, Nvim m, MonadIO m, ReportError e) =>
  String ->
  Either e a ->
  m ()
reportError' _ (Right _) =
  return ()
reportError' componentName (Left e) =
  void $ runExceptT @RpcError $ reportError componentName e

printAllErrors :: (MonadError RpcError m, MonadRibo m, Nvim m, MonadIO m) => m ()
printAllErrors = do
  errors <- Ribo.getErrors
  liftIO $ putDoc (pretty errors <> line)

runRiboReport ::
  ∀ e s m.
  (MonadReader (Ribosome s) m, MonadRibo m, Nvim m, MonadIO m, ReportError e) =>
  String ->
  RiboE s e m () ->
  m ()
runRiboReport componentName =
  reportError' componentName <=< runRiboE