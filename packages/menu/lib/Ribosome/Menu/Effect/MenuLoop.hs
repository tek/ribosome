module Ribosome.Menu.Effect.MenuLoop where

import Conc (PScoped)
import Streamly.Prelude (SerialT)

import Ribosome.Menu.Data.CursorIndex (CursorIndex)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuItems (MenuItems)
import Ribosome.Menu.Data.RenderMenu (RenderMenu)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data MenuLoop filter i :: Effect where
  WithRender :: (RenderMenu i -> m ()) -> m a -> MenuLoop filter i m a
  UseCursor :: (CursorIndex -> m (CursorIndex, a)) -> MenuLoop filter i m a
  ReadCursor :: MenuLoop filter i m CursorIndex
  UseItems :: (MenuItems filter i -> m (MenuItems filter i, a)) -> MenuLoop filter i m a
  ReadItems :: MenuLoop filter i m (MenuItems filter i)
  ChangeFilter :: filter -> MenuLoop filter i m ()
  StartPrompt :: MenuLoop filter i m ()
  WaitPrompt :: MenuLoop filter i m ()
  PromptQuit :: MenuLoop filter i m ()
  PromptUpdated :: Prompt -> MenuLoop filter i m ()
  PromptLooped :: MenuLoop filter i m ()
  Render :: MenuLoop filter i m ()

makeSem ''MenuLoop

type MenuLoops filter i =
  PScoped (SerialT IO (MenuItem i), filter) () (MenuLoop filter i)
