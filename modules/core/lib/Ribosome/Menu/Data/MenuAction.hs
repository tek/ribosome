module Ribosome.Menu.Data.MenuAction where

data MenuAction =
  Quit
  |
  Continue
  |
  Cursor Int
  |
  Submenu Text
  deriving (Eq, Show)
