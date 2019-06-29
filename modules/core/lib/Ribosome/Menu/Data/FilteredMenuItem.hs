module Ribosome.Menu.Data.FilteredMenuItem where

import Ribosome.Menu.Data.MenuItem (MenuItem)

data FilteredMenuItem a =
  FilteredMenuItem {
    _index :: Int,
    _item :: MenuItem a
  }
  deriving (Eq, Show)

makeClassy ''FilteredMenuItem
