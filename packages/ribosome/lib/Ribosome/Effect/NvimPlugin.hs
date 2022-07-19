module Ribosome.Effect.NvimPlugin where

import Polysemy.Bundle (Bundle)

import Ribosome.Effect.VariableWatcher (VariableWatcher)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Effect.Handlers (Handlers)

-- |These effects comprise the three core concepts that define a Neovim plugin:
--
-- - 'Handlers' represents the basic RPC handlers that respond to functions, commands and autocommands
--
-- - 'VariableWatcher' is a reactive system that is triggered by several frequently sent autocommands and inspects a
-- user-defined set of Neovim variables for changes. When a variable's value has been observed to have changed from the
-- previously recorded state, the associated handler is executed.
--
-- - 'MappingHandler' is a slight variation of 'Handlers' that uses a declarative 'Ribosome.Mapping' to allow scratch
-- buffers to defined Neovim mappings in an ergonomic way.
type NvimPluginEffects =
  [Handlers !! HandlerError, VariableWatcher !! HandlerError]

-- |A 'Bundle' that groups 'NvimPluginEffects'.
newtype NvimPlugin :: Effect where
  NvimPlugin :: { unNvimPlugin :: Bundle NvimPluginEffects m a } -> NvimPlugin m a
