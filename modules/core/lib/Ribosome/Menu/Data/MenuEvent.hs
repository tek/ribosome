module Ribosome.Menu.Data.MenuEvent where

import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data MenuEvent =
  Init Prompt
  |
  PromptChange Text Prompt
  |
  Mapping Text Prompt
  |
  NewItems MenuItem
  |
  Quit
  deriving (Eq, Show)
