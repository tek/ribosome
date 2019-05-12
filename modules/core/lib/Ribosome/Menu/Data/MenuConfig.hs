module Ribosome.Menu.Data.MenuConfig where

import Conduit (ConduitT)

import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuUpdate (MenuUpdate)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)

data MenuConfig m =
  MenuConfig {
    _items :: ConduitT () MenuItem m (),
    _handle :: MenuUpdate -> m Menu,
    _render :: MenuUpdate -> m (),
    _prompt :: PromptConfig m
  }
