-- |Persisting data across vim sessions
module Ribosome.Effect.Persist where

import Path (File, Path, Rel)

-- |This effect abstracts storing data of type @a@ in the file system to allow loading it when a plugin starts.
--
-- Each distinct type corresponds to a separate copy of this effect. When the same type should be stored in separate
-- files for different components of the plugin, use 'Tagged'.
-- The subdirectory or file name used for a type is specified to the interpreter.
-- If the constructor is called with a file name, each value is stored in a separate file, otherwise the same file is
-- overwritten on every call to 'store'.
--
-- The default interpreter delegates file path resolution to the effect 'Ribosome.Persist.PersistPath' and uses JSON to
-- codec the data.
data Persist a :: Effect where
  -- |Store a value in the persistence file or, if the first argument is 'Just', in that file in the persistence
  -- directory.
  Store :: Maybe (Path Rel File) -> a -> Persist a m ()
  -- |Load a value from the persistence file or, if the first argument is 'Just', from that file in the persistence
  -- directory.
  -- Returns 'Nothing' if the file doesn't exist.
  Load :: Maybe (Path Rel File) -> Persist a m (Maybe a)

makeSem ''Persist

-- |Load a value from the persistence file or, if the first argument is 'Just', from that file in the persistence
-- directory.
-- Returns the fallback value if the file doesn't exist.
loadOr ::
  Member (Persist a) r =>
  Maybe (Path Rel File) ->
  a ->
  Sem r a
loadOr path a =
  fromMaybe a <$> load path

-- |Load a value from the persistence file.
-- Returns 'Nothing' if the file doesn't exist.
loadSingle ::
  Member (Persist a) r =>
  Sem r (Maybe a)
loadSingle =
  load Nothing

-- |Load a value from the persistence file.
-- Returns the fallback value if the file doesn't exist.
loadSingleOr ::
  Member (Persist a) r =>
  a ->
  Sem r a
loadSingleOr a =
  fromMaybe a <$> loadSingle

-- |Load a value from the specified file in the persistence directory.
-- Returns 'Nothing' if the file doesn't exist.
loadPath ::
  Member (Persist a) r =>
  Path Rel File ->
  Sem r (Maybe a)
loadPath path =
  load (Just path)

-- |Load a value from the specified file in the persistence directory.
-- Returns the fallback value if the file doesn't exist.
loadPathOr ::
  Member (Persist a) r =>
  Path Rel File ->
  a ->
  Sem r a
loadPathOr path a =
  fromMaybe a <$> loadPath path
