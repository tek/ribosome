module Ribosome.Effect.VariableWatcher where

data VariableWatcher :: Effect where
  Update :: VariableWatcher m a

makeSem ''VariableWatcher
