module Ribosome.Effect.BuiltinHandlers where

import Ribosome.Data.Mapping (MappingIdent)

data BuiltinHandlers :: Effect where
  Variables :: BuiltinHandlers m ()
  Mapping :: MappingIdent -> BuiltinHandlers m ()

makeSem ''BuiltinHandlers
