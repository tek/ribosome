module Ribosome.Menu.Action where

import Conc (Lock)
import Data.Generics.Labels ()
import Lens.Micro.Mtl (use, (%=))

import Ribosome.Menu.Combinators (numVisible, overEntries)
import Ribosome.Menu.Data.CursorIndex (CursorIndex (CursorIndex))
import qualified Ribosome.Menu.Data.MenuAction as MenuAction
import Ribosome.Menu.Data.MenuAction (MenuAction)
import Ribosome.Menu.Data.MenuState (
  CursorLock,
  MenuSem,
  MenuStack,
  MenuStateEffects,
  MenuWidget,
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
  MenuWidget r a
act =
  pure . Just

menuIgnore ::
  MenuWidget r a
menuIgnore =
  pure Nothing

menuOk ::
  MenuWidget r a
menuOk =
  pure (Just MenuAction.Continue)

menuRender ::
  MenuWidget r a
menuRender =
  act MenuAction.Render

menuQuit ::
  MenuWidget r a
menuQuit =
  act MenuAction.abort

menuSuccess ::
  a ->
  MenuWidget r a
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
  MenuWidget r a
menuModify action = do
  menuWrite action
  menuRender

menuNavigate ::
  Members (MenuStateEffects i) r =>
  Member (Tagged CursorLock Lock) r =>
  MenuSem i r () ->
  MenuWidget r a
menuNavigate action = do
  menuRead action
  menuRender

menuCycle ::
  Members (MenuStateEffects i) r =>
  Member (Tagged CursorLock Lock) r =>
  Int ->
  MenuWidget r a
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
  MenuWidget r a
menuToggle =
  menuModify toggleSelected

menuToggleAll ::
  Members (MenuStack i) r =>
  MenuWidget r a
menuToggleAll =
  menuModify $ semState do
    entries %= overEntries (const (#selected %~ not))

menuUpdatePrompt ::
  Prompt ->
  MenuWidget r a
menuUpdatePrompt prompt =
  act (MenuAction.UpdatePrompt prompt)
