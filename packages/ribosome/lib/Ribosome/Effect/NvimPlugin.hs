module Ribosome.Effect.NvimPlugin where

import Ribosome.Data.Mapping (MappingIdent)

import Ribosome.Effect.MappingHandler (MappingHandler)
import Ribosome.Effect.VariableWatcher (VariableWatcher)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Effect.Handlers (Handlers)

data NvimPlugin :: Effect where
  Mapping :: MappingIdent -> NvimPlugin m Bool

makeSem ''NvimPlugin

type NvimPlugin' =
  [Handlers !! HandlerError, VariableWatcher !! HandlerError, MappingHandler !! HandlerError]
