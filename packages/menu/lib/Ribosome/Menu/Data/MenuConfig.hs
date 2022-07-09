module Ribosome.Menu.Data.MenuConfig where

import Streamly.Prelude (SerialT)

import Ribosome.Menu.Data.MenuItem (MenuItem)

data MenuConfig i =
  MenuConfig {
    items :: SerialT IO (MenuItem i)
  }
  deriving stock (Generic)
