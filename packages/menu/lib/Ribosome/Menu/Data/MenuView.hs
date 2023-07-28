module Ribosome.Menu.Data.MenuView where

import Ribosome.Menu.Data.CursorIndex (CursorIndex)
import Ribosome.Menu.Data.CursorLine (CursorLine)

newtype EntryIndex =
  EntryIndex Word
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

data ViewRange =
  ViewRange {
    bottom :: EntryIndex,
    top :: EntryIndex,
    cursorLine :: CursorLine
  }
  deriving stock (Eq, Show, Generic)

data MenuView =
  MenuView {
    range :: Maybe ViewRange,
    cursor :: CursorIndex
  }
  deriving stock (Eq, Show, Generic)

instance Default MenuView where
  def =
    MenuView Nothing 0
