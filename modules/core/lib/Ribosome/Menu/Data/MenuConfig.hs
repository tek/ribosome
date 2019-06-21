module Ribosome.Menu.Data.MenuConfig where

import Conduit (ConduitT)

import Ribosome.Menu.Data.MenuConsumer (MenuConsumer)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuRenderEvent (MenuRenderEvent)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)

data MenuConfig m a i =
  MenuConfig {
    _items :: ConduitT () [MenuItem i] m (),
    _handle :: MenuConsumer m a i,
    _render :: MenuRenderEvent m a i -> m (),
    _prompt :: PromptConfig m
  }
