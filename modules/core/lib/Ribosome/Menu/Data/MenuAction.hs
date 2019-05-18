module Ribosome.Menu.Data.MenuAction where

import Ribosome.Menu.Data.MenuEvent (QuitReason)

data MenuAction =
  Quit QuitReason
  |
  Continue
  |
  Cursor Int
  |
  Submenu Text
  deriving (Eq, Show)
