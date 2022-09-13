module Ribosome.Menu.Effect.MenuFilter where

import Ribosome.Menu.Data.Entry (Entries, Entry)
import Ribosome.Menu.Data.MenuItem (Items, MenuItem)
import Ribosome.Menu.Data.State (MenuQuery)

data FilterJob i :: Type -> Type where
  Match :: Int -> MenuItem i -> FilterJob i (Maybe (Int, Entry i))
  Initial :: Items i -> FilterJob i (Entries i)
  Refine :: Entries i -> FilterJob i (Entries i)

data MenuFilter (mode :: Type -> Type) :: Effect where
  MenuFilter :: mode i -> MenuQuery -> FilterJob i a -> MenuFilter mode m a

makeSem ''MenuFilter
