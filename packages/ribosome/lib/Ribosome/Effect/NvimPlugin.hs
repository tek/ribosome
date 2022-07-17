module Ribosome.Effect.NvimPlugin where

import Polysemy.Bundle (Bundle)

import Ribosome.Effect.MappingHandler (MappingHandler)
import Ribosome.Effect.VariableWatcher (VariableWatcher)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Effect.Handlers (Handlers)

-- |These effects comprise the three core concepts that define a Neovim plugin:
--
-- - 'Handlers' represents the basic RPC handlers that respond to functions, commands and autocommands
--
-- - 'VariableWatcher' is a reactive system that reacts to several frequently sent autocommands and inspects a
-- user-defined set of Neovim variables for changes. When a variable's value has been observed to have changed from the
-- previously recorded state, the associated handler is executed.
--
-- - 'MappingHandler' is a slight variation of 'Handlers' that makes defining dynamic mappings a bit more ergonomic.
type NvimPluginEffects =
  [Handlers !! HandlerError, VariableWatcher !! HandlerError, MappingHandler !! HandlerError]

-- |A 'Bundle' that groups 'NvimPluginEffects'.
newtype NvimPlugin :: Effect where
  NvimPlugin :: { unNvimPlugin :: Bundle NvimPluginEffects m a } -> NvimPlugin m a
