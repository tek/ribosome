module Ribosome.Menu.Effect.PromptEvents where

import Ribosome.Menu.Prompt.Data.PromptMode (PromptMode)
import Ribosome.Menu.Prompt.Data.PromptUpdate (PromptUpdate)

data PromptEvents :: Effect where
  HandlePromptEvent :: Text -> PromptMode -> PromptEvents m PromptUpdate

makeSem ''PromptEvents
