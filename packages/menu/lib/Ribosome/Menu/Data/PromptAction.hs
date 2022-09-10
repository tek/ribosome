module Ribosome.Menu.Data.PromptAction where

import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data PromptAction f a =
  Continue
  |
  Quit a
  |
  Update Prompt
  |
  ChangeFilter f
  deriving stock (Eq, Show)
