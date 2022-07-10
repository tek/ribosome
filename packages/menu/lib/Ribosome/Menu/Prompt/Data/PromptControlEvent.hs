module Ribosome.Menu.Prompt.Data.PromptControlEvent where

import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data PromptControlEvent =
  Set Prompt
  |
  Render
  |
  Quit 
  deriving stock (Eq, Show)
