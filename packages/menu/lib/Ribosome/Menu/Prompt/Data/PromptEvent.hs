module Ribosome.Menu.Prompt.Data.PromptEvent where

import Ribosome.Data.Mapping (MappingLhs)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data PromptEvent =
  Update Prompt
  |
  Mapping MappingLhs
  |
  Quit (Maybe Text)
  |
  Ignore
  deriving stock (Eq, Show)
