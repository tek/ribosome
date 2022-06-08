module Ribosome.Menu.Effect.MenuConsumer where

import Ribosome.Menu.Data.MenuAction (MenuAction)
import Ribosome.Menu.Data.MenuEvent (MenuEvent)

data MenuConsumer a :: Effect where
  MenuConsumerEvent :: MenuEvent -> MenuConsumer a m (Maybe (MenuAction a))

makeSem ''MenuConsumer
