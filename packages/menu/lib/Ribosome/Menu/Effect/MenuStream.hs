module Ribosome.Menu.Effect.MenuStream where

import Streamly.Prelude (SerialT)

import Ribosome.Menu.Data.MenuAction (MenuAction)
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)

data MenuStream :: Effect where
  MenuStream ::
    SerialT IO (MenuItem i) ->
    SerialT IO (Prompt, PromptEvent) ->
    (MenuAction result -> m ()) ->
    (Prompt -> PromptEvent -> m (Maybe MenuEvent)) ->
    m MenuEvent ->
    ([MenuItem i] -> m MenuEvent) ->
    m () ->
    (MenuEvent -> m (MenuAction result)) ->
    (MenuAction result -> m (Maybe (MenuResult result))) ->
    MenuStream m (Maybe (MenuResult result))

makeSem ''MenuStream
