module Ribosome.PluginName where

import Exon (exon)

import Ribosome.Data.PluginName (PluginName (PluginName))
import Ribosome.Text (capitalize)

pluginName ::
  Member (Reader PluginName) r =>
  Sem r PluginName
pluginName =
  ask

pluginNamePrefixed ::
  Member (Reader PluginName) r =>
  Text ->
  Sem r Text
pluginNamePrefixed msg = do
  PluginName name <- pluginName
  pure [exon|#{name}: #{msg}|]

pluginNameCapitalized ::
  Member (Reader PluginName) r =>
  Sem r PluginName
pluginNameCapitalized = do
  PluginName n <- pluginName
  pure (PluginName (capitalize n))
