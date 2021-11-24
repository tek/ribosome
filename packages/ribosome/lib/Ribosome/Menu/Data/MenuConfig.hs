module Ribosome.Menu.Data.MenuConfig where

import Streamly.Prelude (SerialT)

import Ribosome.Menu.Data.MenuConsumer (MenuConsumer)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuRenderEvent (MenuRenderEvent)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)

data MenuConfig m a i =
  MenuConfig {
    _items :: SerialT m [MenuItem i],
    _handle :: MenuConsumer m a i,
    _render :: MenuRenderEvent m a i -> m (),
    _prompt :: PromptConfig m,
    _maxItems :: Maybe Int
  }

makeClassy ''MenuConfig
