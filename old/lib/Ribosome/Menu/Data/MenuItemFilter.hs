module Ribosome.Menu.Data.MenuItemFilter where

import Ribosome.Menu.Data.Entry (Entries, Entry)
import Ribosome.Menu.Data.MenuItem (Items, MenuItem)
import Ribosome.Menu.Data.MenuData (MenuQuery)

data MenuItemFilter a =
  MenuItemFilter {
    match :: Text -> Int -> MenuItem a -> Maybe (Int, Entry a),
    initial :: MenuQuery -> Items a -> Entries a,
    refine :: MenuQuery -> Entries a -> Entries a
  }
