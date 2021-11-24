module Ribosome.Menu.Data.FilteredMenuItem where

import Ribosome.Menu.Data.MenuItem (MenuItem (_abbreviated))

data FilteredMenuItem a =
  FilteredMenuItem {
    _index :: Int,
    _item :: MenuItem a
  }
  deriving stock (Eq, Show)

makeClassy ''FilteredMenuItem

instance Eq a => Ord (FilteredMenuItem a) where
  compare =
    comparing _index <> comparing (_abbreviated . _item)
