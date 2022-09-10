module Ribosome.Menu.Action where

import Lens.Micro.Mtl (view)

import qualified Ribosome.Menu.Class.FilterEnum as FilterEnum
import Ribosome.Menu.Class.FilterEnum (FilterEnum)
import Ribosome.Menu.Combinators (numVisible, overEntries)
import Ribosome.Menu.Data.CursorIndex (CursorIndex (CursorIndex))
import qualified Ribosome.Menu.Data.MenuAction as MenuAction
import Ribosome.Menu.Data.MenuAction (MenuAction)
import Ribosome.Menu.Effect.MenuState (MenuState, itemsState, modifyCursor, readCursor, readItems, viewItems)
import Ribosome.Menu.Lens ((%=))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

type MenuSem f i r a =
  Sem (MenuState f i : Reader Prompt : r) a

type MenuWidget f i r a =
  MenuSem f i r (Maybe (MenuAction f a))

act ::
  MenuAction f a ->
  Sem r (Maybe (MenuAction f a))
act =
  pure . Just

menuIgnore :: MenuWidget f i r a
menuIgnore =
  pure Nothing

menuOk :: Sem r (Maybe (MenuAction f a))
menuOk =
  pure (Just MenuAction.Continue)

menuRender :: Sem r (Maybe (MenuAction f a))
menuRender =
  act MenuAction.Render

menuQuit :: Sem r (Maybe (MenuAction f a))
menuQuit =
  act MenuAction.abort

menuSuccess ::
  a ->
  Sem r (Maybe (MenuAction f a))
menuSuccess ma =
  act (MenuAction.success ma)

cycleMenu ::
  Int ->
  MenuSem f i r ()
cycleMenu offset = do
  count <- viewItems (to numVisible)
  modifyCursor \ current -> fromMaybe 0 ((current + fromIntegral offset) `mod` fromIntegral count)

menuModify ::
  MenuSem f i r () ->
  MenuWidget f i r a
menuModify action = do
  action
  menuRender

menuNavigate ::
  MenuSem f i r () ->
  MenuWidget f i r a
menuNavigate action = do
  action
  menuRender

menuCycle ::
  Int ->
  MenuWidget f i r a
menuCycle offset =
  menuNavigate (cycleMenu offset)

toggleSelected ::
  MenuSem f i r ()
toggleSelected = do
  itemsState do
    CursorIndex cur <- readCursor
    #entries %= overEntries \ index ->
      if index == cur
      then #selected %~ not
      else id
  cycleMenu 1

menuToggle ::
  MenuWidget f i r a
menuToggle =
  menuModify toggleSelected

menuToggleAll ::
  MenuWidget f i r a
menuToggleAll =
  menuModify $ itemsState do
    #entries %= overEntries (const (#selected %~ not))

menuUpdatePrompt ::
  Prompt ->
  MenuWidget f i r a
menuUpdatePrompt prompt =
  act (MenuAction.UpdatePrompt prompt)

menuChangeFilter ::
  f ->
  MenuWidget f i r a
menuChangeFilter f =
  act (MenuAction.ChangeFilter f)

menuCycleFilter ::
  FilterEnum f =>
  MenuWidget f i r a
menuCycleFilter = do
  cur <- view #currentFilter <$> readItems
  act (MenuAction.ChangeFilter (FilterEnum.cycle cur))
