module Ribosome.Menu.Effect.MenuFilter where

import Ribosome.Menu.Class.MenuMode (MenuMode)
import Ribosome.Menu.Data.Entry (Entries, Entry)
import Ribosome.Menu.Data.MenuItem (Items, MenuItem)
import Ribosome.Menu.Data.MenuQuery (MenuQuery)

data FilterJob i :: Type -> Type where
  Match :: Int -> MenuItem i -> FilterJob i (Maybe (Int, Entry i))
  Initial :: Items i -> FilterJob i (Entries i)
  Refine :: Entries i -> FilterJob i (Entries i)

data MenuFilter :: Effect where
  MenuFilter :: MenuMode i mode => mode -> MenuQuery -> FilterJob i a -> MenuFilter m a

makeSem ''MenuFilter
