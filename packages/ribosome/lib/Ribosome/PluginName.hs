module Ribosome.PluginName where

import Ribosome.Data.PluginName (PluginName (PluginName))
import Ribosome.Text (capitalize)

pluginName ::
  Member (Reader PluginName) r =>
  Sem r PluginName
pluginName =
  ask

pluginNameCapitalized ::
  Member (Reader PluginName) r =>
  Sem r PluginName
pluginNameCapitalized = do
  PluginName n <- pluginName
  pure (PluginName (capitalize n))
