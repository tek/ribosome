-- |Interpreters for @'Reader' 'Plugin'@
module Ribosome.Interpreter.PluginName where

import Ribosome.Data.PluginConfig (PluginConfig (PluginConfig, name))
import Ribosome.Data.PluginName (PluginName)

-- |Interpret @'Reader' 'PluginName'@ by extracting the name from the plugin config provided by another @'Reader'
-- 'PluginConfig'@.
--
-- This interpreter is used by the main function machinery.
interpretPluginName ::
  Member (Reader PluginConfig) r =>
  InterpreterFor (Reader PluginName) r
interpretPluginName sem =
  ask >>= \ PluginConfig {name} ->
    runReader name sem
