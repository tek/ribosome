module Ribosome.Menu.Data.BasicMenuAction where

import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuEvent (QuitReason)

data BasicMenuChange =
  NoChange
  |
  Change
  |
  Reset
  deriving (Eq, Show)

data BasicMenuAction m a i =
  Continue BasicMenuChange (Menu i)
  |
  Quit (QuitReason m a)
  deriving (Show)
