module Ribosome.Effect.VariableWatcher where

import Data.MessagePack (Object)

import Ribosome.Data.WatchedVariable (WatchedVariable)

data VariableWatcher :: Effect where
  WatchedVariables :: VariableWatcher m [WatchedVariable]
  Update :: Object -> WatchedVariable -> VariableWatcher m ()
  Unwatch :: WatchedVariable -> VariableWatcher m ()

makeSem ''VariableWatcher
