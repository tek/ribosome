module Ribosome.Menu.Effect.MenuStream where

import Streamly.Prelude (AsyncT, SerialT)

import Ribosome.Menu.Data.MenuAction (MenuAction)
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)

data MenuStream i :: Effect where
  MenuStream ::
    AsyncT IO (Prompt, PromptEvent) ->
    (MenuAction result -> m ()) ->
    (Prompt -> PromptEvent -> m (Maybe MenuEvent)) ->
    m MenuEvent ->
    ([MenuItem i] -> m MenuEvent) ->
    m () ->
    (MenuEvent -> m (MenuAction result)) ->
    (MenuAction result -> m (Maybe (MenuResult result))) ->
    MenuStream i m (SerialT IO (MenuResult result))

makeSem ''MenuStream
