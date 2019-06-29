module Ribosome.Menu.Data.MenuItemFilter where

import Ribosome.Menu.Data.FilteredMenuItem (FilteredMenuItem)
import Ribosome.Menu.Data.MenuItem (MenuItem)

newtype MenuItemFilter a =
  MenuItemFilter (Text -> [MenuItem a] -> [FilteredMenuItem a])
