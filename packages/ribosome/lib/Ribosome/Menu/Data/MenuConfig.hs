module Ribosome.Menu.Data.MenuConfig where

import Control.Lens (makeClassy)
import Streamly.Prelude (SerialT)

import Ribosome.Menu.Data.MenuConsumer (MenuConsumer)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuItemFilter (MenuItemFilter)
import Ribosome.Menu.Data.MenuRenderer (MenuRenderer)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)

data MenuConfig m i a =
  MenuConfig {
    _items :: SerialT m (MenuItem i),
    _itemFilter :: MenuItemFilter i,
    _consumer :: MenuConsumer m i a,
    _render :: MenuRenderer m i,
    _prompt :: PromptConfig m
  }

makeClassy ''MenuConfig
