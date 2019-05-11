module Ribosome.Menu.Data.MenuUpdate where

import Ribosome.Menu.Data.MenuContent (MenuContent)
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import Ribosome.Menu.Data.Prompt (Prompt)

data MenuUpdate =
  MenuUpdate {
    _event :: MenuEvent,
    _content :: MenuContent,
    _prompt :: Prompt
  }
  deriving (Eq, Show)
