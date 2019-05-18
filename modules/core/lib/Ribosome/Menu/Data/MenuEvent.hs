module Ribosome.Menu.Data.MenuEvent where

import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data QuitReason m a =
  Aborted
  |
  PromptError Text
  |
  NoOutput
  |
  Return a
  |
  Execute (m a)

data MenuEvent m a =
  Init Prompt
  |
  PromptChange Text Prompt
  |
  Mapping Text Prompt
  |
  NewItems MenuItem
  |
  Quit (QuitReason m a)
