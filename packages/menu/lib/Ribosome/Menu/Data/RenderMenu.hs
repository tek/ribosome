module Ribosome.Menu.Data.RenderMenu where

import Ribosome.Menu.Data.CursorIndex (CursorIndex)
import Ribosome.Menu.Data.Entry (Entries)
import qualified Ribosome.Menu.Data.Menu as Menu
import Ribosome.Menu.Data.Menu (Menu (Menu))
import qualified Ribosome.Menu.Data.MenuItems as MenuItems
import Ribosome.Menu.Data.MenuItems (MenuItems (MenuItems))

data RenderMenu i =
  RenderMenu {
    entries :: Entries i,
    cursor :: CursorIndex
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)

fromMenu :: Menu filter i -> RenderMenu i
fromMenu Menu {items = MenuItems {entries}, cursor} =
  RenderMenu {..}
