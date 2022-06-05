module Ribosome.Interpreter.PluginName where

import Ribosome.Data.PluginConfig (PluginConfig (PluginConfig, name))
import Ribosome.Data.PluginName (PluginName)

interpretPluginName ::
  Member (Reader PluginConfig) r =>
  InterpreterFor (Reader PluginName) r
interpretPluginName sem =
  ask >>= \ PluginConfig {name} ->
    runReader name sem
