module Ribosome.Menu.Prompt.Data.PromptUpdate where

import Ribosome.Menu.Prompt.Data.CursorUpdate (CursorUpdate)
import Ribosome.Menu.Prompt.Data.PromptMode (PromptMode)
import Ribosome.Menu.Prompt.Data.TextUpdate (TextUpdate)

data PromptUpdate =
  Modify PromptMode CursorUpdate TextUpdate
  |
  Quit
  |
  Ignore
  deriving stock (Eq, Show)
