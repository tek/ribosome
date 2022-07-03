module Ribosome.Menu.Data.MenuConfig where

import Streamly.Prelude (SerialT)

import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuItemFilter (MenuItemFilter)
import Ribosome.Menu.Prompt.Data.PromptFlag (PromptFlag)

data MenuConfig i =
  MenuConfig {
    items :: SerialT IO (MenuItem i),
    itemFilter :: Maybe (MenuItemFilter i),
    flags :: [PromptFlag]
  }
  deriving stock (Generic)
