module Ribosome.Menu.Data.MenuConsumer where

import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuAction (MenuAction)
import Ribosome.Menu.Data.MenuUpdate (MenuUpdate)

newtype MenuConsumer m a =
  MenuConsumer { unMenuConsumer :: MenuUpdate m a -> m (MenuAction m a, Menu) }
