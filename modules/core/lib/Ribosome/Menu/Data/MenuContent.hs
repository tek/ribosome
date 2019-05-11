module Ribosome.Menu.Data.MenuContent where

import Ribosome.Menu.Data.MenuItem (MenuItem)

newtype MenuContent =
  MenuContent {
    _items :: [MenuItem]
  }
  deriving (Eq, Show)
