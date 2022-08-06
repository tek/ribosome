module Ribosome.Menu.Data.PromptAction where

import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data PromptAction a =
  Continue
  |
  Quit a
  |
  Update Prompt
  deriving stock (Eq, Show)
