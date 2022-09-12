module Ribosome.Menu.Data.RenderMenu where

import Ribosome.Menu.Class.FilterEnum (FilterEnum (describe))
import Ribosome.Menu.Data.CursorIndex (CursorIndex (CursorIndex))
import Ribosome.Menu.Data.Entry (Entries)
import qualified Ribosome.Menu.Data.Menu as Menu
import Ribosome.Menu.Data.Menu (Menu (Menu))
import qualified Ribosome.Menu.Data.MenuItems as MenuItems
import Ribosome.Menu.Data.MenuItems (MenuItems (MenuItems))
import Ribosome.Menu.Data.MenuStatus (MenuStatus (MenuStatus))

data RenderMenu i =
  RenderMenu {
    entries :: Entries i,
    cursor :: CursorIndex,
    status :: MenuStatus
  }
  deriving stock (Eq, Show, Generic)

menuStatus ::
  FilterEnum filter =>
  Int ->
  Int ->
  CursorIndex ->
  filter ->
  MenuStatus
menuStatus itemCount entryCount (CursorIndex cursor) f =
  MenuStatus (describe f) itemCount entryCount cursor

fromMenu ::
  FilterEnum filter =>
  Menu filter i ->
  RenderMenu i
fromMenu Menu {items = MenuItems {entries, itemCount, entryCount, currentFilter}, cursor} =
  RenderMenu {status = menuStatus itemCount entryCount cursor currentFilter, ..}
