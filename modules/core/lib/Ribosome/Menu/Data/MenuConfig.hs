module Ribosome.Menu.Data.MenuConfig where

import Conduit (ConduitT)
import Control.Concurrent.STM.TMChan (TMChan)

import Ribosome.Menu.Data.MenuConsumer (MenuConsumer)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuRenderEvent (MenuRenderEvent)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)

data MenuConfig m a i =
  MenuConfig {
    _items :: ConduitT () [MenuItem i] m (),
    _handle :: TMChan PromptEvent -> MenuConsumer m a i,
    _render :: MenuRenderEvent m a i -> m (),
    _prompt :: PromptConfig m,
    _maxItems :: Maybe Int
  }

makeClassy ''MenuConfig
