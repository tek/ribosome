module Ribosome.Menu.Action where

import Ribosome.Menu.Class.MenuState (MenuState, MenuState (Item, Mode, mode))
import qualified Ribosome.Menu.Class.MenuMode as MenuMode
import Ribosome.Menu.Combinators (numVisible, overEntries)
import Ribosome.Menu.Data.CursorIndex (CursorIndex (CursorIndex))
import qualified Ribosome.Menu.Data.MenuAction as MenuAction
import Ribosome.Menu.Data.MenuAction (MenuAction)
import Ribosome.Menu.Effect.Menu (Menu, basicState, menuState, modifyCursor, readCursor, viewState)
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
  modifyCursor \ current -> fromMaybe 0 ((current + fromIntegral offset) `mod` fromIntegral count)

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
    CursorIndex cur <- readCursor
    #entries %= overEntries \ index ->
      if index == cur
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
