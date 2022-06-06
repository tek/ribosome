module Ribosome.Effect.NvimPlugin where

import Polysemy.Bundle (Bundle)

import Ribosome.Effect.MappingHandler (MappingHandler)
import Ribosome.Effect.VariableWatcher (VariableWatcher)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Effect.Handlers (Handlers)

type NvimPluginEffects =
  [Handlers !! HandlerError, VariableWatcher !! HandlerError, MappingHandler !! HandlerError]

newtype NvimPlugin :: Effect where
  NvimPlugin :: { unNvimPlugin :: Bundle NvimPluginEffects m a } -> NvimPlugin m a
