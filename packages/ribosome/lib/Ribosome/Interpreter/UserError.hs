module Ribosome.Interpreter.UserError where

import Ribosome.Data.PluginName (PluginName)
import Ribosome.Host.Effect.UserError (UserError (UserError))
import Ribosome.PluginName (pluginNamePrefixed)

interpretUserErrorPrefixed ::
  Member (Reader PluginName) r =>
  InterpreterFor UserError r
interpretUserErrorPrefixed =
  interpret \case
    UserError e _ ->
      Just . pure <$> pluginNamePrefixed e
