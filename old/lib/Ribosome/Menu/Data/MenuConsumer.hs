module Ribosome.Menu.Data.MenuConsumer where

import Ribosome.Menu.Data.MenuAction (MenuAction)
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import Ribosome.Menu.Data.MenuState (MenuM, MenuState, MenuStateM)

type MenuWidget m i a =
  MenuStateM m i (Maybe (MenuAction m a))

type MenuWidgetM m i a =
  MenuM m i (Maybe (MenuAction m a))

newtype MenuApp m i a =
  MenuApp { unMenuApp :: MenuEvent -> MenuWidget m i a }

newtype MenuConsumer m i a =
  MenuConsumer { unMenuConsumer :: MenuState i -> MenuEvent -> m (Maybe (MenuAction m a)) }
