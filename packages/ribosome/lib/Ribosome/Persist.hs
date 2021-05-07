module Ribosome.Persist where

import Control.Exception (IOException, try)
import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict', encodeFile)
import Path (Abs, Dir, File, Path, Rel, addExtension, parent, parseAbsDir, parseRelDir, toFilePath, (</>))
import Path.IO (XdgDirectory(XdgCache), createDirIfMissing, doesFileExist, getXdgDir)

import Ribosome.Config.Setting (setting)
import qualified Ribosome.Config.Settings as S (persistenceDir)
import Ribosome.Control.Monad.Error (recoveryFor)
import Ribosome.Control.Monad.Ribo
import Ribosome.Data.PersistError (PersistError)
import qualified Ribosome.Data.PersistError as PersistError (PersistError(..))
import Ribosome.Data.SettingError (SettingError)

defaultPersistencePath :: MonadIO m => m (Path Abs Dir)
defaultPersistencePath =
  liftIO $ getXdgDir XdgCache Nothing

persistencePath ::
  MonadRibo m =>
  NvimE e m =>
  MonadThrow m =>
  MonadDeepError e SettingError m =>
  Path Rel File ->
  m (Path Abs File)
persistencePath path = do
  base <- defaultPersistencePath `recoveryFor` (parseAbsDir =<< setting S.persistenceDir)
  name <- parseRelDir . toString =<< pluginName
  return $ base </> name </> path

persistenceFile ::
  MonadRibo m =>
  NvimE e m =>
  MonadThrow m =>
  MonadDeepError e SettingError m =>
  Path Rel File ->
  m (Path Abs File)
persistenceFile path = do
  file <- persistencePath path
  createDirIfMissing True (parent file)
  addExtension ".json" file

persistStore ::
  MonadRibo m =>
  NvimE e m =>
  MonadThrow m =>
  MonadDeepError e SettingError m =>
  ToJSON a =>
  Path Rel File ->
  a ->
  m ()
persistStore path a = do
  file <- persistenceFile path
  liftIO $ encodeFile (toFilePath file) a

noSuchFile :: MonadDeepError e PersistError m => Path Abs File -> m a
noSuchFile = throwHoist . PersistError.NoSuchFile . toFilePath

ensureExistence ::
  MonadIO m =>
  MonadDeepError e PersistError m =>
  Path Abs File ->
  m ()
ensureExistence file = do
  exists <- liftIO $ doesFileExist file
  unless exists (noSuchFile file)

decodeError ::
  MonadDeepError e PersistError m =>
  Path Abs File ->
  Text ->
  m a
decodeError file = throwHoist . PersistError.Decode (toFilePath file)

fileNotReadable ::
  MonadDeepError e PersistError m =>
  Path Abs File ->
  IOException ->
  m (Either String a)
fileNotReadable file _ = throwHoist $ PersistError.FileNotReadable (toFilePath file)

safeDecodeFile ::
  MonadIO m =>
  MonadDeepError e PersistError m =>
  FromJSON a =>
  Path Abs File ->
  m a
safeDecodeFile file = do
  result <- either (fileNotReadable file) return =<< (liftIO . try . eitherDecodeFileStrict' . toFilePath $ file)
  either (decodeError file) return . mapLeft toText $ result

persistLoad ::
  MonadIO m =>
  MonadRibo m =>
  NvimE e m =>
  MonadThrow m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  FromJSON a =>
  Path Rel File ->
  m a
persistLoad path = do
  file <- persistenceFile path
  ensureExistence file
  safeDecodeFile file

mayPersistLoad ::
  MonadRibo m =>
  NvimE e m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  MonadThrow m =>
  FromJSON a =>
  Path Rel File ->
  m (Maybe a)
mayPersistLoad =
  catchAt recover . persistLoad
  where
    recover (PersistError.NoSuchFile _) =
      return Nothing
    recover e =
      throwHoist e
