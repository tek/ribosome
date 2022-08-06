module Ribosome.Menu.Action where

import Ribosome.Menu.Combinators (numVisible, overEntries)
import Ribosome.Menu.Data.CursorIndex (CursorIndex (CursorIndex))
import qualified Ribosome.Menu.Data.MenuAction as MenuAction
import Ribosome.Menu.Data.MenuAction (MenuAction)
import Ribosome.Menu.Effect.MenuState (MenuState, itemsState, modifyCursor, readCursor, viewItems)
import Ribosome.Menu.Lens ((%=))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

type MenuSem i r a =
  Sem (MenuState i : Reader Prompt : r) a

type MenuWidget i r a =
  MenuSem i r (Maybe (MenuAction a))

act ::
  MenuAction a ->
  Sem r (Maybe (MenuAction a))
act =
  pure . Just

menuIgnore :: MenuWidget i r a
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
  Int ->
  MenuSem i r ()
cycleMenu offset = do
  count <- viewItems (to numVisible)
  modifyCursor \ current -> fromMaybe 0 ((current + fromIntegral offset) `mod` fromIntegral count)

menuModify ::
  MenuSem i r () ->
  MenuWidget i r a
menuModify action = do
  action
  menuRender

menuNavigate ::
  MenuSem i r () ->
  MenuWidget i r a
menuNavigate action = do
  action
  menuRender

menuCycle ::
  Int ->
  MenuWidget i r a
menuCycle offset =
  menuNavigate (cycleMenu offset)

toggleSelected ::
  MenuSem i r ()
toggleSelected = do
  itemsState do
    CursorIndex cur <- readCursor
    #entries %= overEntries \ index ->
      if index == cur
      then #selected %~ not
      else id
  cycleMenu 1

menuToggle ::
  MenuWidget i r a
menuToggle =
  menuModify toggleSelected

menuToggleAll ::
  MenuWidget i r a
menuToggleAll =
  menuModify $ itemsState do
    #entries %= overEntries (const (#selected %~ not))

menuUpdatePrompt ::
  Prompt ->
  MenuWidget i r a
menuUpdatePrompt prompt =
  act (MenuAction.UpdatePrompt prompt)
