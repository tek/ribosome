module Ribosome.Menu.Data.MenuConfig where

import Streamly.Prelude (SerialT)

import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuItemFilter (MenuItemFilter)

data MenuConfig i =
  MenuConfig {
    items :: SerialT IO (MenuItem i),
    itemFilter :: Maybe (MenuItemFilter i)
  }
  deriving stock (Generic)
