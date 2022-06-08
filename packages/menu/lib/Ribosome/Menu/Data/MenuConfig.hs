module Ribosome.Menu.Data.MenuConfig where

import Streamly.Prelude (SerialT)

import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuItemFilter (MenuItemFilter)
import Ribosome.Menu.Data.MenuRenderer (MenuRenderer, hoistMenuRenderer)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig, hoistPromptConfig)

data MenuConfig r i a =
  MenuConfig {
    items :: SerialT IO (MenuItem i),
    itemFilter :: MenuItemFilter i,
    render :: MenuRenderer r i,
    prompt :: PromptConfig r
  }
  deriving stock (Generic)

hoistMenuConfig ::
  (∀ x . Sem r x -> Sem r' x) ->
  MenuConfig r i a ->
  MenuConfig r' i a
hoistMenuConfig f MenuConfig {..} =
  MenuConfig {
    render = hoistMenuRenderer f render,
    prompt = hoistPromptConfig f prompt,
    ..
  }
