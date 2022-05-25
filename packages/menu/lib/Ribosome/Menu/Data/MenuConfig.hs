module Ribosome.Menu.Data.MenuConfig where

import Control.Lens (makeClassy)
import Streamly.Prelude (SerialT)

import Ribosome.Menu.Data.MenuConsumer (MenuConsumer, hoistMenuConsumer)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuItemFilter (MenuItemFilter)
import Ribosome.Menu.Data.MenuRenderer (MenuRenderer)
import Ribosome.Menu.Data.MenuState (MenuStateSem)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig)

data MenuConfig r m i a =
  MenuConfig {
    _items :: SerialT m (MenuItem i),
    _itemFilter :: MenuItemFilter i,
    _consumer :: MenuConsumer r i a,
    _render :: MenuRenderer m i,
    _prompt :: PromptConfig m
  }

makeClassy ''MenuConfig

hoistMenuConfig ::
  (∀ x . Sem r x -> Sem r' x) ->
  (∀ x . MenuStateSem r i x -> MenuStateSem r' i x) ->
  MenuConfig r m i a ->
  MenuConfig r' m i a
hoistMenuConfig f g MenuConfig {..} =
  MenuConfig {_consumer = hoistMenuConsumer f g _consumer, ..}
