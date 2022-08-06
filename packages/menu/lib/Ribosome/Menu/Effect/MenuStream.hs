module Ribosome.Menu.Effect.MenuStream where

import Streamly.Prelude (SerialT)

import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.RenderEvent (RenderEvent)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data MenuStream :: Effect where
  MenuStream ::
    SerialT IO (MenuItem i) ->
    m (Maybe Prompt) ->
    (Prompt -> m RenderEvent) ->
    ([MenuItem i] -> m ()) ->
    (RenderEvent -> m ()) ->
    (MenuEvent -> m ()) ->
    MenuStream m ()

makeSem ''MenuStream
