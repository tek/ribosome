-- |Interpreters for 'PersistPath'
module Ribosome.Interpreter.PersistPath where

import Path (Abs, Dir, Path, Rel, parseRelDir, (</>))
import Path.IO (XdgDirectory (XdgCache), createDirIfMissing, getXdgDir)

import Ribosome.Data.PersistPathError (PersistPathError (Permissions, Undefined))
import Ribosome.Data.PluginName (PluginName (unPluginName))
import Ribosome.Data.SettingError (SettingError)
import qualified Ribosome.Effect.PersistPath as PersistPath
import Ribosome.Effect.PersistPath (PersistPath (PersistPath))
import qualified Ribosome.Effect.Settings as Settings
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.BootError (BootError)
import Ribosome.PluginName (pluginName)

-- |Append an optional subdir to a dir.
maybeSubdir :: Path b Dir -> Maybe (Path Rel Dir) -> Path b Dir
maybeSubdir root = \case
  Just sub ->
    root </> sub
  Nothing ->
    root

-- |Append an optional subdir to a dir and ensure existence of the resulting directory if 'True' is given.
persistPath ::
  Members [Stop PersistPathError, Embed IO] r =>
  Bool ->
  Path Abs Dir ->
  Maybe (Path Rel Dir) ->
  Sem r (Path Abs Dir)
persistPath create base sub = do
  when create (stopTryAny (const (Permissions path)) (createDirIfMissing True path))
  pure path
  where
    path =
      maybeSubdir base sub

-- |Interpret 'PersistPath' by using the specified root directory.
interpretPersistPathAt ::
  Member (Embed IO) r =>
  Bool ->
  Path Abs Dir ->
  InterpreterFor (PersistPath !! PersistPathError) r
interpretPersistPathAt create base =
  interpretResumable \case
    PersistPath sub ->
      persistPath create base sub

-- |Look up the XDG cache directory, returning 'Nothing' if it is unavailable.
xdgCache ::
  Member (Embed IO) r =>
  Sem r (Maybe (Path Abs Dir))
xdgCache =
  rightToMaybe <$> tryAny (getXdgDir XdgCache Nothing)

-- |Interpret 'PersistPath' by reading the global setting for the root directory, or using the given directory if the
-- variable is unset.
--
-- The given @name@ is appended to the root, which usually identifies the plugin.
interpretPersistPathSetting ::
  Members [Settings !! SettingError, Embed IO] r =>
  Bool ->
  Maybe (Path Abs Dir) ->
  Path Rel Dir ->
  InterpreterFor (PersistPath !! PersistPathError) r
interpretPersistPathSetting create fallback name =
  interpretResumable \case
    PersistPath sub -> do
      base <- stopNote Undefined . (<|> fallback) =<< Settings.maybe PersistPath.setting
      persistPath create (base </> name) sub

-- |Interpret 'PersistPath' by reading the global setting for the root directory, or using the XDG cache directory if
-- the variable is unset.
--
-- The plugin name is used as a subdir of the root.
--
-- This uses 'Resumable', see [Errors]("Ribosome#g:errors").
interpretPersistPath ::
  Members [Settings !! SettingError, Reader PluginName, Error BootError, Embed IO] r =>
  Bool ->
  InterpreterFor (PersistPath !! PersistPathError) r
interpretPersistPath create sem = do
  xdg <- xdgCache
  name <- note "plugin name not suitable for file system paths" . parseRelDir . toString . (.unPluginName) =<< pluginName
  interpretPersistPathSetting create xdg name sem
