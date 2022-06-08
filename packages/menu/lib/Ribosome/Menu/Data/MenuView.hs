module Ribosome.Menu.Data.MenuView where

import Ribosome.Menu.Data.CursorIndex (CursorIndex)
import Ribosome.Menu.Data.CursorLine (CursorLine)

data MenuView =
  MenuView {
    topIndex :: Int,
    botIndex :: Int,
    cursor :: CursorIndex,
    cursorLine :: CursorLine
  }
  deriving stock (Eq, Show, Generic)

instance Default MenuView where
  def =
    MenuView 0 0 0 0
