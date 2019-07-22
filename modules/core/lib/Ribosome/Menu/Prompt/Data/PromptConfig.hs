module Ribosome.Menu.Prompt.Data.PromptConfig where

import Conduit (ConduitT)
import Control.Lens (view)

import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import Ribosome.Menu.Prompt.Data.PromptRenderer (PromptRenderer)
import Ribosome.Menu.Prompt.Data.PromptState (PromptState)
import Ribosome.Menu.Prompt.Data.PromptUpdate (PromptUpdate)

data PromptFlag =
  StartInsert
  |
  OnlyInsert
  deriving (Eq, Show)

data PromptConfig m =
  PromptConfig {
    _source :: ConduitT () PromptEvent m (),
    _modes :: [PromptFlag] -> PromptEvent -> PromptState -> m PromptUpdate,
    _render :: PromptRenderer m,
    _flags :: [PromptFlag]
  }

makeClassy ''PromptConfig

class TestPromptFlag a where
  promptFlag :: PromptFlag -> a -> Bool

instance TestPromptFlag [PromptFlag] where
  promptFlag =
    elem

instance TestPromptFlag (PromptConfig m) where
  promptFlag flag =
    promptFlag flag . view flags

startInsert :: TestPromptFlag a => a -> Bool
startInsert =
  promptFlag StartInsert

onlyInsert :: TestPromptFlag a => a -> Bool
onlyInsert =
  promptFlag OnlyInsert
