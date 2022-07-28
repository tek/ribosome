module Ribosome.Interpreter.UserError where

import Log (Severity (Info))

import Ribosome.Data.PluginName (PluginName)
import Ribosome.Host.Effect.UserError (UserError (UserError))
import Ribosome.PluginName (pluginNamePrefixed)

interpretUserErrorPrefixed ::
  Member (Reader PluginName) r =>
  InterpreterFor UserError r
interpretUserErrorPrefixed =
  interpret \case
    UserError e severity | severity >= Info ->
      Just . pure <$> pluginNamePrefixed e
    UserError _ _ ->
      pure Nothing
