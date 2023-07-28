module Ribosome.Menu.Action where

import qualified Ribosome.Menu.Class.MenuMode as MenuMode
import Ribosome.Menu.Class.MenuState (MenuState (Item, Mode, mode))
import Ribosome.Menu.Combinators (numVisible, overEntries)
import qualified Ribosome.Menu.Data.MenuAction as MenuAction
import Ribosome.Menu.Data.MenuAction (MenuAction)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Effect.Menu (Menu, basicState, menuState, modifyCursor, readCursor, viewState)
import Ribosome.Menu.ItemLens (focus)
import Ribosome.Menu.Lens (use, (%=), (.=))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

type MenuActionSem r a =
  Sem r (Maybe (MenuAction a))

type MenuSem s r a =
  Sem (Menu s : Reader Prompt : r) a

type MenuWidget s r a =
  MenuSem s r (Maybe (MenuAction a))

act ::
  MenuAction a ->
  Sem r (Maybe (MenuAction a))
act =
  pure . Just

menuIgnore :: MenuWidget s r a
menuIgnore =
  pure Nothing

menuOk :: Sem r (Maybe (MenuAction a))
menuOk =
  pure (Just MenuAction.Continue)

menuRender :: Sem r (Maybe (MenuAction a))
menuRender =
  act MenuAction.Render

menuQuit :: Sem r (Maybe (MenuAction a))
menuQuit =
  act MenuAction.abort

menuSuccess ::
  a ->
  Sem r (Maybe (MenuAction a))
menuSuccess ma =
  act (MenuAction.success ma)

cycleMenu ::
  MenuState s =>
  Int ->
  MenuSem s r ()
cycleMenu offset = do
  count <- viewState (to numVisible)
  modifyCursor \ current ->
    let
      new :: Int
      new = fromIntegral current + offset
    in fromIntegral (fromMaybe 0 (new `mod` count))

menuCycle ::
  MenuState s =>
  Int ->
  MenuWidget s r a
menuCycle offset = do
  cycleMenu offset
  menuRender

-- TODO this can:
-- - Return when the focus was found
-- - Skip over entries in the map that are shorter than (cursor - acc)
-- - be refactored into @overFocus@
toggleSelected ::
  MenuState s =>
  MenuSem s r ()
toggleSelected = do
  basicState do
    cur <- readCursor
    #entries %= overEntries \ index ->
      if index == fromIntegral cur
      then #selected %~ not
      else id
  cycleMenu 1

menuToggle ::
  MenuState s =>
  MenuWidget s r a
menuToggle = do
  toggleSelected
  menuRender

menuToggleAll ::
  MenuState s =>
  MenuWidget s r a
menuToggleAll = do
  basicState do
    #entries %= overEntries (const (#selected %~ not))
  menuRender

menuUpdatePrompt ::
  Prompt ->
  MenuActionSem r a
menuUpdatePrompt prompt =
  act (MenuAction.UpdatePrompt prompt)

menuChangeMode ::
  MenuState s =>
  Mode s ->
  MenuWidget s r a
menuChangeMode m = do
  menuState do
    mode .= m
  menuRender

menuCycleFilter ::
  ∀ s r a .
  MenuState s =>
  MenuWidget s r a
menuCycleFilter = do
  menuState do
    cur <- use mode
    mode .= (MenuMode.cycleFilter @(Item s) cur)
  menuRender

menuFocusItem ::
  MenuState s =>
  MenuWidget s r (Maybe (MenuItem (Item s)))
menuFocusItem =
  menuState do
    selection <- use focus
    menuSuccess selection
