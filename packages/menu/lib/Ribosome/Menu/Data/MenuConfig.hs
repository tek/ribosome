module Ribosome.Menu.Data.MenuConfig where

import Streamly.Prelude (SerialT)

import Ribosome.Menu.Data.MenuConsumer (MenuConsumer, hoistMenuConsumer)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuItemFilter (MenuItemFilter)
import Ribosome.Menu.Data.MenuRenderer (MenuRenderer, hoistMenuRenderer)
import Ribosome.Menu.Data.MenuState (MenuStateSem)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig, hoistPromptConfig)

data MenuConfig r i a =
  MenuConfig {
    items :: SerialT IO (MenuItem i),
    itemFilter :: MenuItemFilter i,
    consumer :: MenuConsumer i r a,
    render :: MenuRenderer r i,
    prompt :: PromptConfig r
  }
  deriving stock (Generic)

hoistMenuConfig ::
  (∀ x . Sem r x -> Sem r' x) ->
  (∀ x . MenuStateSem i r x -> MenuStateSem i r' x) ->
  MenuConfig r i a ->
  MenuConfig r' i a
hoistMenuConfig f g MenuConfig {..} =
  MenuConfig {
    consumer = hoistMenuConsumer f g consumer,
    render = hoistMenuRenderer f render,
    prompt = hoistPromptConfig f prompt,
    ..
  }
