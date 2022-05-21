module Ribosome.Data.PluginState where

import Data.MessagePack (Object)

import Ribosome.Data.Scratch (Scratch)

data PluginState =
  PluginState {
    scratch :: Map Text Scratch,
    watchedVariables :: Map Text Object
  }
  deriving stock (Eq, Show)
