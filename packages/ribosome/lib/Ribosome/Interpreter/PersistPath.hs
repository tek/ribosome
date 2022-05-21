module Ribosome.Interpreter.PersistPath where

import Path (Abs, Dir, Path, Rel, parseRelDir, (</>))
import Path.IO (XdgDirectory (XdgCache), getXdgDir)

import Ribosome.Data.PersistPathError (PersistPathError (Undefined))
import Ribosome.Data.PluginName (PluginName (unPluginName))
import Ribosome.Data.SettingError (SettingError)
import qualified Ribosome.Effect.PersistPath as PersistPath
import Ribosome.Effect.PersistPath (PersistPath (PersistPath))
import qualified Ribosome.Effect.Settings as Settings
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.BootError (BootError)
import Ribosome.PluginName (pluginName)

interpretPersistPathAt ::
  Path Abs Dir ->
  InterpreterFor PersistPath r
interpretPersistPathAt path =
  interpret \case
    PersistPath ->
      pure path

xdgCache ::
  Member (Embed IO) r =>
  Sem r (Maybe (Path Abs Dir))
xdgCache =
  rightToMaybe <$> tryAny (getXdgDir XdgCache Nothing)

interpretPersistPathSetting ::
  Member (Settings !! SettingError) r =>
  Maybe (Path Abs Dir) ->
  Path Rel Dir ->
  InterpreterFor (PersistPath !! PersistPathError) r
interpretPersistPathSetting fallback name =
  interpretResumable \case
    PersistPath -> do
      base <- stopNote Undefined . (<|> fallback) =<< Settings.maybe PersistPath.setting
      pure (base </> name)

interpretPersistPath ::
  Members [Settings !! SettingError, Reader PluginName, Error BootError, Embed IO] r =>
  InterpreterFor (PersistPath !! PersistPathError) r
interpretPersistPath sem = do
  xdg <- xdgCache
  name <- note "plugin name not suitable for file system paths" . parseRelDir . toString . unPluginName =<< pluginName
  interpretPersistPathSetting xdg name sem
