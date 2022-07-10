module Ribosome.Menu.Effect.NvimPromptInput where

import Ribosome.Menu.Prompt.Data.InputEvent (InputEvent)

data NvimPromptInput :: Effect where
  GetChar :: m () -> NvimPromptInput m InputEvent

makeSem ''NvimPromptInput
