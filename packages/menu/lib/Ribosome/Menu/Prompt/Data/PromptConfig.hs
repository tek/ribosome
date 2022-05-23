module Ribosome.Menu.Prompt.Data.PromptConfig where

import Control.Lens (makeClassy, view)
import Streamly.Prelude (SerialT)

import Ribosome.Menu.Prompt.Data.PromptInputEvent (PromptInputEvent)
import Ribosome.Menu.Prompt.Data.PromptRenderer (PromptRenderer)
import Ribosome.Menu.Prompt.Data.PromptState (PromptState)
import Ribosome.Menu.Prompt.Data.PromptUpdate (PromptUpdate)

data PromptFlag =
  StartInsert
  |
  OnlyInsert
  deriving stock (Eq, Show)

newtype PromptInput m =
  PromptInput { unPromptInput :: MVar () -> SerialT m PromptInputEvent }

newtype PromptEventHandler m =
  PromptEventHandler { unPromptEventHandler :: PromptInputEvent -> PromptState -> m PromptUpdate }

data PromptConfig m =
  PromptConfig {
    _source :: PromptInput m,
    _handleEvent :: [PromptFlag] -> PromptEventHandler m,
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
