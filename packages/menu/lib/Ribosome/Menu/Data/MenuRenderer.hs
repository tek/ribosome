module Ribosome.Menu.Data.MenuRenderer where

import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuRenderEvent (MenuRenderEvent)

newtype MenuRenderer r i =
  MenuRenderer { unMenuRenderer :: Menu i -> MenuRenderEvent -> Sem r () }

hoistMenuRenderer ::
  (∀ x . Sem r x -> Sem r' x) ->
  MenuRenderer r i ->
  MenuRenderer r' i
hoistMenuRenderer f (MenuRenderer r) =
  MenuRenderer \ m e -> f (r m e)
