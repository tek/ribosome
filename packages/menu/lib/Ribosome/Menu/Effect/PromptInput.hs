module Ribosome.Menu.Effect.PromptInput where

import Ribosome.Menu.Prompt.Data.InputEvent (InputEvent)

data PromptInput :: Effect where
  Event :: PromptInput m InputEvent

makeSem ''PromptInput
