module Ribosome.Menu.Data.PromptAction where

import Ribosome.Menu.Prompt.Data.Prompt (PromptState)

data PromptAction a =
  Continue
  |
  Quit a
  |
  Update PromptState
  deriving stock (Eq, Show)
