module Ribosome.Menu.Effect.PromptEvents where

import Ribosome.Menu.Prompt.Data.PromptInputEvent (PromptInputEvent)
import Ribosome.Menu.Prompt.Data.PromptMode (PromptMode)
import Ribosome.Menu.Prompt.Data.PromptUpdate (PromptUpdate)

data PromptEvents :: Effect where
  HandlePromptEvent :: PromptInputEvent -> PromptMode -> PromptEvents m PromptUpdate

makeSem ''PromptEvents
