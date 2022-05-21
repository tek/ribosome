module Ribosome.Interpreter.Persist where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict', encodeFile)
import Exon (exon)
import Path (Abs, Dir, File, Path, parseRelFile, toFilePath, (</>))
import Path.IO (createDirIfMissing, doesFileExist)

import qualified Ribosome.Data.PersistError as PersistError
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Data.PersistPathError (PersistPathError)
import qualified Ribosome.Effect.Persist as Persist
import Ribosome.Effect.Persist (Persist)
import Ribosome.Effect.PersistPath (PersistPath, persistPath)
import Ribosome.Host.Data.BootError (BootError (BootError))
import Ribosome.Host.Interpret (with)
import Ribosome.Path (pathText)

persistBase ::
  Members [PersistPath !! PersistPathError, Stop PersistError] r =>
  Sem r (Path Abs Dir)
persistBase =
  resumeHoist PersistError.Path persistPath

loadFile ::
  FromJSON a =>
  Members [Stop PersistError, Embed IO] r =>
  Path Abs File ->
  Sem r a
loadFile file = do
  stopEitherWith decodeFailed =<< stopNote notReadable =<< tryMaybe (eitherDecodeFileStrict' (toFilePath file))
  where
    notReadable =
      PersistError.Permission (pathText file)
    decodeFailed =
      PersistError.Decode (pathText file) . toText

interpretPersist ::
  ToJSON a =>
  FromJSON a =>
  Members [PersistPath !! PersistPathError, Error BootError, Embed IO] r =>
  Text ->
  InterpreterFor (Persist a !! PersistError) r
interpretPersist name =
  with parse \ rel ->
    interpretResumable \case
      Persist.Store a -> do
        base <- persistBase
        stopNote (PersistError.Permission (pathText base)) =<< tryMaybe (createDirIfMissing True base)
        embed (encodeFile (toFilePath (base </> rel)) a)
      Persist.Load -> do
        file <- (</> rel) <$> persistBase
        ifM (fromMaybe False <$> tryMaybe (doesFileExist file)) (loadFile file) (pure Nothing)
  where
    parse =
      note (BootError [exon|Invalid persist name: #{name}|]) (parseRelFile (toString [exon|#{name}.json|]))
