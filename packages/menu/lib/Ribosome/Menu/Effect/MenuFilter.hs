module Ribosome.Menu.Effect.MenuFilter where

import Ribosome.Menu.Data.Entry (Entries, Entry)
import Ribosome.Menu.Data.MenuItems (MenuQuery)
import Ribosome.Menu.Data.MenuItem (Items, MenuItem)

data MenuFilter (style :: Type) :: Effect where
  Match :: style -> Text -> Int -> MenuItem i -> MenuFilter style m (Maybe (Int, Entry i))
  Initial :: style -> MenuQuery -> Items i -> MenuFilter style m (Entries i)
  Refine :: style -> MenuQuery -> Entries i -> MenuFilter style m (Entries i)

makeSem ''MenuFilter
