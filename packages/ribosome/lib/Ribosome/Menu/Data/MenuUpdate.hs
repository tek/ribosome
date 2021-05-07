module Ribosome.Menu.Data.MenuUpdate where

import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuEvent (MenuEvent)

data MenuUpdate m a i =
  MenuUpdate {
    _event :: MenuEvent m a i,
    _menu :: Menu i
  }

makeClassy ''MenuUpdate
