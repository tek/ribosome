module Ribosome.Effect.BuiltinHandlers where

data BuiltinHandlers :: Effect where
  Variables :: BuiltinHandlers m ()

makeSem ''BuiltinHandlers
