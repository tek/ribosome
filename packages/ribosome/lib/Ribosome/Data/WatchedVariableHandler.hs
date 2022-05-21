module Ribosome.Data.WatchedVariableHandler where

import Data.MessagePack (Object)

import Ribosome.Data.WatchedVariable (WatchedVariable)

data WatchedVariableHandler r =
  WatchedVariableHandler {
    var :: WatchedVariable,
    handler :: Object -> Sem r ()
  }
