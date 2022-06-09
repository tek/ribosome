module Ribosome.Menu.Action where

import Control.Lens (to, use, (%=), (%~))
import Data.Generics.Labels ()

import Ribosome.Menu.Combinators (numVisible, overEntries)
import Ribosome.Menu.Data.CursorIndex (CursorIndex (CursorIndex))
import qualified Ribosome.Menu.Data.MenuAction as MenuAction
import Ribosome.Menu.Data.MenuAction (MenuAction)
import Ribosome.Menu.Data.MenuState (
  CursorLock,
  MenuSem,
  MenuStateEffects,
  MenuStack,
  MenuWidget',
  SemS (SemS),
  menuRead,
  menuWrite,
  semState,
  unSemS,
  )
import Ribosome.Menu.ItemLens (cursor, entries)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

act ::
  MenuAction a ->
  Sem r (Maybe (MenuAction a))
act =
  pure . Just

menuIgnore ::
  Sem r (Maybe (MenuAction a))
menuIgnore =
  pure Nothing

menuOk ::
  Sem r (Maybe (MenuAction a))
menuOk =
  pure (Just MenuAction.Continue)

menuRender ::
  Sem r (Maybe (MenuAction a))
menuRender =
  act MenuAction.Render

menuQuit ::
  Sem r (Maybe (MenuAction a))
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
  unSemS do
    count <- use (to numVisible)
    cursor %= \ currentCount -> fromMaybe 0 ((currentCount + fromIntegral offset) `mod` fromIntegral count)

menuModify ::
  Members (MenuStack i) r =>
  MenuSem i r () ->
  MenuWidget' r a
menuModify action = do
  menuWrite action
  menuRender

menuNavigate ::
  Members (MenuStateEffects i) r =>
  Member (Sync CursorLock) r =>
  MenuSem i r () ->
  MenuWidget' r a
menuNavigate action = do
  menuRead action
  menuRender

menuCycle ::
  Members (MenuStateEffects i) r =>
  Member (Sync CursorLock) r =>
  Int ->
  MenuWidget' r a
menuCycle offset =
  menuNavigate (cycleMenu offset)

toggleSelected ::
  MenuSem i r ()
toggleSelected = do
  semState do
    CursorIndex cur <- use cursor
    entries %= overEntries \ index ->
      if index == cur
      then #selected %~ not
      else id
    SemS (cycleMenu 1)

menuToggle ::
  Members (MenuStack i) r =>
  MenuWidget' r a
menuToggle =
  menuModify toggleSelected

menuToggleAll ::
  Members (MenuStack i) r =>
  MenuWidget' r a
menuToggleAll =
  menuModify $ semState do
    entries %= overEntries (const (#selected %~ not))

menuUpdatePrompt ::
  Prompt ->
  MenuWidget' r a
menuUpdatePrompt prompt =
  act (MenuAction.UpdatePrompt prompt)
