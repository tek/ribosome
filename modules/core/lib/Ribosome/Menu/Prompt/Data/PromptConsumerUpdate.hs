module Ribosome.Menu.Prompt.Data.PromptConsumerUpdate where

import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)

data PromptConsumerUpdate =
  PromptConsumerUpdate {
    _event :: PromptEvent,
    _prompt :: Prompt
  }
  deriving (Eq, Show)
