module Ribosome.Menu.Data.MenuView where

import Ribosome.Menu.Data.CursorIndex (CursorIndex)
import Ribosome.Menu.Data.CursorLine (CursorLine)

data MenuView =
  MenuView {
    _topIndex :: Int,
    _botIndex :: Int,
    _cursor :: CursorIndex,
    _cursorLine :: CursorLine
  }
  deriving stock (Eq, Show)

makeClassy ''MenuView

instance Default MenuView where
  def =
    MenuView 0 0 0 0
