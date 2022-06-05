module Ribosome.Effect.MappingHandler where

import Ribosome.Data.Mapping (MappingIdent)

data MappingHandler :: Effect where
  Call :: MappingIdent -> MappingHandler m ()

makeSem ''MappingHandler
