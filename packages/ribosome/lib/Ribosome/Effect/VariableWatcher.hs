module Ribosome.Effect.VariableWatcher where

import Ribosome.Data.WatchedVariable (WatchedVariable)

data VariableWatcher :: Effect where
  Update :: VariableWatcher m ()
  Unwatch :: WatchedVariable -> VariableWatcher m ()

makeSem ''VariableWatcher
