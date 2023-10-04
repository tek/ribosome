module Ribosome.Menu.Prompt.Data.PromptEvent where

import Ribosome.Data.Mapping (MappingLhs)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptMode (PromptMode (Insert))

data PromptEvent =
  Update Prompt
  |
  Mapping MappingLhs
  |
  Quit (Maybe Text)
  |
  Ignore
  deriving stock (Eq, Show)

updateInsert :: Prompt -> PromptEvent
updateInsert p =
  Update (p & #mode .~ Insert)
