module Ribosome.Persist(
  persistStore,
  persistenceFile,
  persistencePath,
  defaultPersistencePath,
  persistLoad,
) where

import qualified Control.Lens as Lens (review)
import Control.Monad (unless)
import Control.Monad.Error.Class (MonadError(throwError))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B (readFile, writeFile)
import System.Directory (XdgDirectory(XdgCache), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath (takeDirectory, (</>))
import UnliftIO.Directory (doesFileExist)
import UnliftIO.Exception (tryIO)

import Ribosome.Config.Setting (setting)
import qualified Ribosome.Config.Settings as S (persistenceDir)
import Ribosome.Control.Monad.Error (recoveryFor)
import Ribosome.Control.Monad.Ribo
import Ribosome.Data.DeepError (MonadDeepError(throwHoist))
import Ribosome.Data.PersistError (AsPersistError, _PersistError, PersistError)
import qualified Ribosome.Data.PersistError as PersistError (PersistError(..))
import Ribosome.Data.Setting (SettingError)
import Ribosome.Nvim.Api.RpcCall (AsRpcError, RpcError)

defaultPersistencePath :: MonadIO m => m FilePath
defaultPersistencePath =
  liftIO $ getXdgDirectory XdgCache ""

persistencePath ::
  (MonadRibo m, Nvim m, MonadIO m, MonadDeepError e RpcError m, MonadDeepError e SettingError m) =>
  FilePath ->
  m FilePath
persistencePath path = do
  base <- defaultPersistencePath `recoveryFor` setting S.persistenceDir
  name <- pluginName
  return $ base </> name </> path

persistenceFile ::
  (MonadRibo m, Nvim m, MonadIO m, MonadDeepError e RpcError m, MonadDeepError e SettingError m) =>
  FilePath ->
  m FilePath
persistenceFile path = do
  file <- persistencePath path
  liftIO $ createDirectoryIfMissing True (takeDirectory file)
  return $ file ++ ".json"

persistStore ::
  (MonadRibo m, Nvim m, MonadIO m, MonadDeepError e RpcError m, MonadDeepError e SettingError m) =>
  ToJSON a =>
  FilePath ->
  a ->
  m ()
persistStore path a = do
  file <- persistenceFile path
  liftIO $ B.writeFile file (encode a)

noSuchFile :: MonadDeepError e PersistError m => FilePath -> m a
noSuchFile = throwHoist . PersistError.NoSuchFile

ensureExistence :: (MonadUnliftIO m, MonadDeepError e PersistError m) => FilePath -> m ()
ensureExistence file = do
  exists <- doesFileExist file
  unless exists (noSuchFile file)

safeReadFile ::
  (MonadUnliftIO m, MonadDeepError e RpcError m, MonadDeepError e SettingError m, MonadDeepError e PersistError m) =>
  FilePath ->
  m ByteString
safeReadFile file =
  either err return =<< (tryIO . liftIO . B.readFile $ file)
  where
    err _ = throwHoist $ PersistError.FileNotReadable file

decodeError ::
  (MonadDeepError e RpcError m, MonadDeepError e SettingError m, MonadDeepError e PersistError m) =>
  FilePath ->
  String ->
  m a
decodeError = curry $ throwHoist . uncurry PersistError.Decode

persistLoad ::
  (MonadRibo m, Nvim m, MonadUnliftIO m, MonadDeepError e RpcError m, MonadDeepError e SettingError m, MonadDeepError e PersistError m) =>
  FromJSON a =>
  FilePath ->
  m a
persistLoad path = do
  file <- persistenceFile path
  ensureExistence file
  json <- safeReadFile file
  either (decodeError path) return . eitherDecode $ json
