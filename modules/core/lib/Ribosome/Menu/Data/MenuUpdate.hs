module Ribosome.Menu.Data.MenuUpdate where

import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data MenuUpdate =
  MenuUpdate {
    _event :: MenuEvent,
    _menu :: Menu
  }
  deriving (Eq, Show)
