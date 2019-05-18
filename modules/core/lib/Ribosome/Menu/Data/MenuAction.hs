module Ribosome.Menu.Data.MenuAction where

import Ribosome.Menu.Data.MenuEvent (QuitReason)

data MenuAction m a =
  Quit (QuitReason m a)
  |
  Continue
  |
  Cursor Int
  |
  Submenu Text
