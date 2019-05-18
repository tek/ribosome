module Ribosome.Menu.Data.MenuConfig where

import Conduit (ConduitT)

import Ribosome.Menu.Data.MenuConsumer (MenuConsumer)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuUpdate (MenuUpdate)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)

data MenuConfig m a =
  MenuConfig {
    _items :: ConduitT () MenuItem m (),
    _handle :: MenuConsumer m a,
    _render :: MenuUpdate m a -> m (),
    _prompt :: PromptConfig m
  }
