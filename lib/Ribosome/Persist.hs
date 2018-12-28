module Ribosome.Persist(
  persistStore,
  persistenceFile,
  persistencePath,
  defaultPersistencePath,
  persistLoad,
) where

import GHC.IO.Exception (IOException)
import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT(ExceptT), catchE)
import Data.Aeson (ToJSON, FromJSON, encode, eitherDecode)
import qualified Data.ByteString.Lazy as B (writeFile, readFile, ByteString)
import System.FilePath (takeDirectory, (</>))
import System.Directory (getXdgDirectory, XdgDirectory(XdgCache), createDirectoryIfMissing)
import Ribosome.Monad (liftExceptT)
import Ribosome.Control.Ribo (Ribo)
import qualified Ribosome.Control.Ribo as Ribo (name)
import Ribosome.Config.Setting (settingE)
import qualified Ribosome.Config.Settings as S (persistenceDir)

defaultPersistencePath :: FilePath -> IO FilePath
defaultPersistencePath =
  getXdgDirectory XdgCache

persistencePath :: FilePath -> Ribo e FilePath
persistencePath path = do
  name <- Ribo.name
  let prefixed = name </> path
  custom <- settingE S.persistenceDir
  either (const $ liftIO $ defaultPersistencePath prefixed) (\c -> return $ c </> prefixed) custom

persistenceFile :: FilePath -> Ribo e FilePath
persistenceFile path = do
  file <- persistencePath path
  liftIO $ createDirectoryIfMissing True (takeDirectory file)
  return $ file ++ ".json"

persistStore :: ToJSON a => FilePath -> a -> Ribo e ()
persistStore path a = do
  file <- persistenceFile path
  liftIO $ B.writeFile file (encode a)

noSuchFile :: Monad m => FilePath -> ExceptT String m a
noSuchFile file = ExceptT $ return $ Left $ "persistence file " ++ file ++ " doesn't exist"

safeReadFile :: MonadIO m => FilePath -> m (Either IOException B.ByteString)
safeReadFile file = liftIO $ try $ B.readFile file

persistLoad :: FromJSON a => FilePath -> ExceptT String (Ribo e) a
persistLoad path = do
  file <- liftExceptT $ persistenceFile path
  json <- catchE (ExceptT $ safeReadFile file) (const $ noSuchFile file)
  ExceptT $ return $ eitherDecode json
