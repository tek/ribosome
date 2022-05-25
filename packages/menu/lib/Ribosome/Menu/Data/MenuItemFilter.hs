module Ribosome.Menu.Data.MenuItemFilter where

import Ribosome.Menu.Data.Entry (Entries, Entry)
import Ribosome.Menu.Data.MenuData (MenuQuery)
import Ribosome.Menu.Data.MenuItem (Items, MenuItem)

data MenuItemFilter a =
  MenuItemFilter {
    match :: Text -> Int -> MenuItem a -> Maybe (Int, Entry a),
    initial :: MenuQuery -> Items a -> IO (Entries a),
    refine :: MenuQuery -> Entries a -> IO (Entries a)
  }
