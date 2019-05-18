module Ribosome.Menu.Data.MenuConsumer where

import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import Ribosome.Menu.Data.MenuUpdate (MenuUpdate)

newtype MenuConsumer m a =
  MenuConsumer { unMenuConsumer :: MenuUpdate m a -> m (MenuConsumerAction m a, Menu) }