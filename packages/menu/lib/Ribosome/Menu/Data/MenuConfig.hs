module Ribosome.Menu.Data.MenuConfig where

import Streamly.Prelude (SerialT)

import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuItemFilter (MenuItemFilter)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)

data MenuConfig i =
  MenuConfig {
    items :: SerialT IO (MenuItem i),
    itemFilter :: Maybe (MenuItemFilter i),
    prompt :: PromptConfig
  }
  deriving stock (Generic)
