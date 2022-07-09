module Ribosome.Menu.Effect.PromptState where

import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptControlEvent (PromptControlEvent)
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import Ribosome.Menu.Prompt.Data.PromptInputEvent (PromptInputEvent)

data PromptState :: Effect where
  Event :: Either PromptControlEvent PromptInputEvent -> PromptState m (Maybe (Prompt, PromptEvent))
  Set :: Prompt -> PromptState m ()

makeSem ''PromptState
