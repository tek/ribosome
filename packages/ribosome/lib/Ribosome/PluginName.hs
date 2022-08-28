-- |Combinators for 'PluginName'.
module Ribosome.PluginName where

import Exon (exon)

import Ribosome.Data.PluginName (PluginName (PluginName))
import Ribosome.Host.Text (pascalCase)

-- |Get the 'PluginName' from a 'Reader'.
pluginName ::
  Member (Reader PluginName) r =>
  Sem r PluginName
pluginName =
  ask

-- |Get the 'PluginName' from a 'Reader' and prefix the given string with it, followed by a colon..
pluginNamePrefixed ::
  Member (Reader PluginName) r =>
  Text ->
  Sem r Text
pluginNamePrefixed msg = do
  PluginName name <- pluginName
  pure [exon|#{name}: #{msg}|]

-- |Get the 'PluginName' from a 'Reader' and convert it to PascalCase.
pluginNamePascalCase ::
  Member (Reader PluginName) r =>
  Sem r PluginName
pluginNamePascalCase = do
  PluginName n <- pluginName
  pure (PluginName (pascalCase n))
