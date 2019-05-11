module Ribosome.Menu.Prompt.Data.PromptConsumerUpdate where

import Ribosome.Menu.Data.Prompt (Prompt)
import Ribosome.Menu.Data.PromptEvent (PromptEvent)

data PromptConsumerUpdate =
  PromptConsumerUpdate {
    _event :: PromptEvent,
    _prompt :: Prompt
  }
  deriving (Eq, Show)
