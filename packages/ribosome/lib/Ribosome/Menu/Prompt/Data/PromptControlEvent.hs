module Ribosome.Menu.Prompt.Data.PromptControlEvent where
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data PromptControlEvent =
  Set Prompt
  |
  Quit 
  deriving stock (Eq, Show)
