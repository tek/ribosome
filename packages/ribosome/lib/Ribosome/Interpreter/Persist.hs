-- |Interpreters for 'Persist'
module Ribosome.Interpreter.Persist where

import Data.Aeson (eitherDecodeFileStrict', encodeFile)
import Exon (exon)
import qualified Log
import Path (Abs, Dir, File, Path, Rel, parent, parseRelDir, parseRelFile, toFilePath, (</>))
import Path.IO (createDirIfMissing, doesFileExist)

import qualified Ribosome.Data.PersistError as PersistError
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Data.PersistPathError (PersistPathError)
import qualified Ribosome.Effect.Persist as Persist
import Ribosome.Effect.Persist (Persist)
import Ribosome.Effect.PersistPath (PersistPath, persistRoot)
import Ribosome.Host.Data.BootError (BootError (BootError))
import Ribosome.Host.Interpret (with)
import Ribosome.Host.Path (pathText)

-- |Obtain the root directory or stop.
persistBase ::
  Members [PersistPath !! PersistPathError, Stop PersistError] r =>
  Sem r (Path Abs Dir)
persistBase =
  resumeHoist PersistError.Path persistRoot

-- |Load a file and JSON-decode it.
loadFile ::
  FromJSON a =>
  Members [Stop PersistError, Log, Embed IO] r =>
  Path Abs File ->
  Sem r a
loadFile file = do
  Log.debug [exon|Loading persistence file: #{show file}|]
  stopEitherWith decodeFailed =<< stopNote notReadable =<< tryMaybe (eitherDecodeFileStrict' (toFilePath file))
  where
    notReadable =
      PersistError.Permission (pathText file)
    decodeFailed =
      PersistError.Decode (pathText file) . toText

-- |Determine the path to use for a 'Persist' action.
-- If a @subpath@ is given, append it to @dir@, otherwise use @singleFile@.
-- Append the result of the first step to the root dir given by 'PersistPath'.
filepath ::
  Members [PersistPath !! PersistPathError, Stop PersistError] r =>
  Path Rel File ->
  Path Rel Dir ->
  Maybe (Path Rel File) ->
  Sem r (Path Abs File)
filepath singleFile dir subpath = do
  base <- persistBase
  pure (base </> maybe singleFile (dir </>) subpath)

-- |Interpret 'Persist' by writing to the file system.
--
-- Paths are determined as follows:
--
-- - 'PersistPath' defines the root directory for all 'Persist' effects, which might be specific to a plugin, or
-- additionally to entities like the currently edited project (e.g. by directory).
--
-- - The value in the @name@ argument is appended to the root depending on the arguments to the effect constructors.
--
-- - When 'Ribosome.Effect.Persist.store' or 'Ribosome.Effect.Persist.load' are invoked with 'Nothing' for the @subpath@
-- argument, a file named @<name>.json@ is used.
--
-- - When invoked with 'Just' a subpath, a file named @<name>/<subpath>.json@ is used.
--
-- This uses 'Resumable', see [Errors]("Ribosome#errors").
interpretPersist ::
  ToJSON a =>
  FromJSON a =>
  Members [PersistPath !! PersistPathError, Error BootError, Log, Embed IO] r =>
  Text ->
  InterpreterFor (Persist a !! PersistError) r
interpretPersist name =
  with parse \ (singleFile, dir) ->
    interpretResumable \case
      Persist.Store subpath a -> do
        path <- filepath singleFile dir subpath
        let base = parent path
        stopNote (PersistError.Permission (pathText base)) =<< tryMaybe (createDirIfMissing True base)
        embed (encodeFile (toFilePath path) a)
      Persist.Load subpath -> do
        path <- filepath singleFile dir subpath
        Log.debug [exon|Persistence path requested: #{show path}|]
        ifM (fromMaybe False <$> tryMaybe (doesFileExist path)) (loadFile path) (pure Nothing)
  where
    parse =
      note (BootError [exon|Invalid persist name: #{name}|]) namePaths
    namePaths =
      (,) <$> parseRelFile (toString [exon|#{name}.json|]) <*> parseRelDir (toString name)

-- |Interpret 'Persist' by storing nothing.
interpretPersistNull ::
  ∀ a err r .
  InterpreterFor (Persist a !! err) r
interpretPersistNull =
  interpretResumable \case
    Persist.Store _ _ ->
      unit
    Persist.Load _ ->
      pure Nothing
