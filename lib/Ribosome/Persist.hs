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
import Ribosome.Data.PersistError (AsPersistError, _PersistError)
import qualified Ribosome.Data.PersistError as PersistError (PersistError(..))
import Ribosome.Data.Setting (AsSettingError)
import Ribosome.Nvim.Api.RpcCall (AsRpcError)

defaultPersistencePath :: MonadIO m => m FilePath
defaultPersistencePath =
  liftIO $ getXdgDirectory XdgCache ""

persistencePath ::
  (MonadRibo m, Nvim m, MonadIO m, MonadError e m, AsRpcError e, AsSettingError e) =>
  FilePath ->
  m FilePath
persistencePath path = do
  base <- defaultPersistencePath `recoveryFor` setting S.persistenceDir
  name <- pluginName
  return $ base </> name </> path

persistenceFile ::
  (MonadRibo m, Nvim m, MonadIO m, MonadError e m, AsRpcError e, AsSettingError e) =>
  FilePath ->
  m FilePath
persistenceFile path = do
  file <- persistencePath path
  liftIO $ createDirectoryIfMissing True (takeDirectory file)
  return $ file ++ ".json"

persistStore ::
  (MonadRibo m, Nvim m, MonadIO m, MonadError e m, AsRpcError e, AsSettingError e) =>
  ToJSON a =>
  FilePath ->
  a ->
  m ()
persistStore path a = do
  file <- persistenceFile path
  liftIO $ B.writeFile file (encode a)

noSuchFile :: (MonadError e m, AsPersistError e) => FilePath -> m a
noSuchFile = throwError . (Lens.review _PersistError . PersistError.NoSuchFile)

ensureExistence :: (MonadUnliftIO m, MonadError e m, AsPersistError e) => FilePath -> m ()
ensureExistence file = do
  exists <- doesFileExist file
  unless exists (noSuchFile file)

safeReadFile ::
  (MonadUnliftIO m, MonadError e m, AsPersistError e) =>
  FilePath ->
  m ByteString
safeReadFile file =
  either err return =<< (tryIO . liftIO . B.readFile $ file)
  where
    err _ = throwError $ Lens.review _PersistError $ PersistError.FileNotReadable file

decodeError :: (MonadError e m, AsPersistError e) => FilePath -> String -> m a
decodeError = curry $ throwError . (Lens.review _PersistError . uncurry PersistError.Decode)

persistLoad ::
  (MonadRibo m, Nvim m, MonadUnliftIO m, MonadError e m, AsPersistError e, AsSettingError e, AsRpcError e) =>
  FromJSON a =>
  FilePath ->
  m a
persistLoad path = do
  file <- persistenceFile path
  ensureExistence file
  json <- safeReadFile file
  either (decodeError path) return . eitherDecode $ json
