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

maybeSubdir :: Path b Dir -> Maybe (Path Rel Dir) -> Path b Dir
maybeSubdir root = \case
  Just sub ->
    root </> sub
  Nothing ->
    root

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

interpretPersistPathAt ::
  Member (Embed IO) r =>
  Bool ->
  Path Abs Dir ->
  InterpreterFor (PersistPath !! PersistPathError) r
interpretPersistPathAt create base =
  interpretResumable \case
    PersistPath sub ->
      persistPath create base sub

xdgCache ::
  Member (Embed IO) r =>
  Sem r (Maybe (Path Abs Dir))
xdgCache =
  rightToMaybe <$> tryAny (getXdgDir XdgCache Nothing)

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

interpretPersistPath ::
  Members [Settings !! SettingError, Reader PluginName, Error BootError, Embed IO] r =>
  Bool ->
  InterpreterFor (PersistPath !! PersistPathError) r
interpretPersistPath create sem = do
  xdg <- xdgCache
  name <- note "plugin name not suitable for file system paths" . parseRelDir . toString . unPluginName =<< pluginName
  interpretPersistPathSetting create xdg name sem
