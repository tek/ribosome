module Ribosome.Menu.Prompt.Data.PromptEvent where

import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data PromptEvent =
  Update Prompt
  |
  Mapping Text
  |
  Quit (Maybe Text)
  |
  Ignore
  deriving stock (Eq, Show)
