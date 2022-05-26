module Ribosome.Menu.Data.MenuConfig where

import Control.Lens (makeClassy)
import Streamly.Prelude (SerialT)

import Ribosome.Menu.Data.MenuConsumer (MenuConsumer, hoistMenuConsumer)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuItemFilter (MenuItemFilter)
import Ribosome.Menu.Data.MenuRenderer (MenuRenderer, hoistMenuRenderer)
import Ribosome.Menu.Data.MenuState (MenuStateSem)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig, hoistPromptConfig)

data MenuConfig r m i a =
  MenuConfig {
    _items :: SerialT m (MenuItem i),
    _itemFilter :: MenuItemFilter i,
    _consumer :: MenuConsumer i r a,
    _render :: MenuRenderer r i,
    _prompt :: PromptConfig m r
  }

makeClassy ''MenuConfig

hoistMenuConfig ::
  (∀ x . Sem r x -> Sem r' x) ->
  (∀ x . MenuStateSem i r x -> MenuStateSem i r' x) ->
  MenuConfig r m i a ->
  MenuConfig r' m i a
hoistMenuConfig f g MenuConfig {..} =
  MenuConfig {
    _consumer = hoistMenuConsumer f g _consumer,
    _render = hoistMenuRenderer f _render,
    _prompt = hoistPromptConfig f _prompt,
    ..
  }