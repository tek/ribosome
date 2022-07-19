module Ribosome.Effect.NvimPlugin where

import Ribosome.Effect.VariableWatcher (VariableWatcher)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Effect.Handlers (Handlers)

-- |These effects define a Neovim plugin:
--
-- - 'Handlers' represents the basic RPC handlers that respond to functions, commands and autocommands
--
-- - 'VariableWatcher' is a reactive system that is triggered by several frequently sent autocommands and inspects a
-- user-defined set of Neovim variables for changes. When a variable's value has been observed to have changed from the
-- previously recorded state, the associated handler is executed.
type NvimPlugin =
  [Handlers !! HandlerError, VariableWatcher !! HandlerError]
