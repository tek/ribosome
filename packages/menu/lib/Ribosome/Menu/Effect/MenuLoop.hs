module Ribosome.Menu.Effect.MenuLoop where

import Conc (PScoped)
import Streamly.Prelude (SerialT)

import Ribosome.Menu.Data.CursorIndex (CursorIndex)
import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuItems (MenuItems)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data MenuLoop i :: Effect where
  WithRender :: (Menu i -> m ()) -> m a -> MenuLoop i m a
  UseCursor :: (CursorIndex -> m (CursorIndex, a)) -> MenuLoop i m a
  ReadCursor :: MenuLoop i m CursorIndex
  UseItems :: (MenuItems i -> m (MenuItems i, a)) -> MenuLoop i m a
  ReadItems :: MenuLoop i m (MenuItems i)
  StartPrompt :: MenuLoop i m ()
  WaitPrompt :: MenuLoop i m ()
  PromptQuit :: MenuLoop i m ()
  PromptUpdated :: Prompt -> MenuLoop i m ()
  PromptLooped :: MenuLoop i m ()
  Render :: MenuLoop i m ()

makeSem ''MenuLoop

type MenuLoops i =
  PScoped (SerialT IO (MenuItem i)) () (MenuLoop i)
