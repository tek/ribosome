module Ribosome.Menu.Effect.MenuFilter where

import Ribosome.Menu.Data.Entry (Entries, Entry)
import Ribosome.Menu.Data.MenuItems (MenuQuery)
import Ribosome.Menu.Data.MenuItem (Items, MenuItem)

data MenuFilter :: Effect where
  Match :: Text -> Int -> MenuItem i -> MenuFilter m (Maybe (Int, Entry i))
  Initial :: MenuQuery -> Items i -> MenuFilter m (Entries i)
  Refine :: MenuQuery -> Entries i -> MenuFilter m (Entries i)

makeSem ''MenuFilter
