module Ribosome.Persist where

import Control.Exception (IOException, try)
import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict', encodeFile)
import Path (Abs, Dir, File, Path, Rel, addExtension, parent, parseAbsDir, parseRelDir, toFilePath, (</>))
import Path.IO (XdgDirectory(XdgCache), createDirIfMissing, doesFileExist, getXdgDir)

import Ribosome.Config.Setting (setting)
import qualified Ribosome.Config.Settings as S (persistenceDir)
import Ribosome.Control.Monad.Error (recoveryFor)
import Ribosome.Data.PersistError (PersistError)
import qualified Ribosome.Data.PersistError as PersistError (PersistError(..))
import Ribosome.Data.SettingError (SettingError)

defaultPersistencePath :: MonadIO m => m (Path Abs Dir)
defaultPersistencePath =
  liftIO $ getXdgDir XdgCache Nothing

persistencePath ::
  Member Rpc r =>
  MonadThrow m =>
  Path Rel File ->
  m (Path Abs File)
persistencePath path = do
  base <- defaultPersistencePath `recoveryFor` (parseAbsDir =<< setting S.persistenceDir)
  name <- parseRelDir . toString =<< pluginName
  pure $ base </> name </> path

persistenceFile ::
  Member Rpc r =>
  MonadThrow m =>
  Path Rel File ->
  m (Path Abs File)
persistenceFile path = do
  file <- persistencePath path
  createDirIfMissing True (parent file)
  addExtension ".json" file

persistStore ::
  Member Rpc r =>
  MonadThrow m =>
  ToJSON a =>
  Path Rel File ->
  a ->
  m ()
persistStore path a = do
  file <- persistenceFile path
  liftIO $ encodeFile (toFilePath file) a

noSuchFile = throwHoist . PersistError.NoSuchFile . toFilePath

ensureExistence ::
  MonadIO m =>
  Path Abs File ->
  m ()
ensureExistence file = do
  exists <- liftIO $ doesFileExist file
  unless exists (noSuchFile file)

decodeError ::
  Path Abs File ->
  Text ->
  m a
decodeError file = throwHoist . PersistError.Decode (toFilePath file)

fileNotReadable ::
  Path Abs File ->
  IOException ->
  m (Either String a)
fileNotReadable file _ = throwHoist $ PersistError.FileNotReadable (toFilePath file)

safeDecodeFile ::
  MonadIO m =>
  FromJSON a =>
  Path Abs File ->
  m a
safeDecodeFile file = do
  result <- either (fileNotReadable file) pure =<< (liftIO . try . eitherDecodeFileStrict' . toFilePath $ file)
  either (decodeError file) pure . mapLeft toText $ result

persistLoad ::
  MonadIO m =>
  Member Rpc r =>
  MonadThrow m =>
  FromJSON a =>
  Path Rel File ->
  m a
persistLoad path = do
  file <- persistenceFile path
  ensureExistence file
  safeDecodeFile file

mayPersistLoad ::
  Member Rpc r =>
  MonadThrow m =>
  FromJSON a =>
  Path Rel File ->
  m (Maybe a)
mayPersistLoad =
  catchAt recover . persistLoad
  where
    recover (PersistError.NoSuchFile _) =
      pure Nothing
    recover e =
      throwHoist e
