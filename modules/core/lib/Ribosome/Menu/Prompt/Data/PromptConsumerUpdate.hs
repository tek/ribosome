module Ribosome.Menu.Prompt.Data.PromptConsumerUpdate where

import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptConsumed (PromptConsumed)
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)

data PromptConsumerUpdate =
  PromptConsumerUpdate {
    _event :: PromptEvent,
    _prompt :: Prompt,
    _consumed :: PromptConsumed
  }
  deriving (Eq, Show)
