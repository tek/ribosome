module Ribosome.Menu.Data.MenuItemMatcher where

import Ribosome.Menu.Data.MenuItem (MenuItem)

newtype MenuItemMatcher a =
  MenuItemMatcher (Text -> [MenuItem a] -> [MenuItem a])
