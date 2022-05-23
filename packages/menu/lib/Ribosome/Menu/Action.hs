module Ribosome.Menu.Action where

import Control.Lens (to, use, (%=), (%~))

import Ribosome.Menu.Combinators (numVisible, overEntries)
import Ribosome.Menu.Data.CursorIndex (CursorIndex (CursorIndex))
import qualified Ribosome.Menu.Data.Entry as Entry
import qualified Ribosome.Menu.Data.MenuAction as MenuAction
import Ribosome.Menu.Data.MenuAction (MenuAction)
import Ribosome.Menu.Data.MenuConsumer (MenuWidgetSem)
import Ribosome.Menu.Data.MenuData (cursor, entries)
import Ribosome.Menu.Data.MenuStateSem (MenuSem, SemS (SemS), menuRead, menuWriteSem, semState, unSemS)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

act ::
  MenuAction r1 a ->
  Sem r (Maybe (MenuAction r1 a))
act =
  pure . Just

menuIgnore ::
  Sem r (Maybe (MenuAction r1 a))
menuIgnore =
  pure Nothing

menuOk ::
  Sem r (Maybe (MenuAction r1 a))
menuOk =
  pure (Just MenuAction.Continue)

menuRender ::
  Sem r (Maybe (MenuAction r1 a))
menuRender =
  act MenuAction.Render

menuQuit ::
  Sem r (Maybe (MenuAction r1 a))
menuQuit =
  act MenuAction.abort

menuSuccess ::
  Sem r1 a ->
  Sem r (Maybe (MenuAction r1 a))
menuSuccess ma =
  act (MenuAction.success ma)

menuResult ::
  a ->
  Sem r (Maybe (MenuAction r1 a))
menuResult =
  menuSuccess . pure

cycleMenu ::
  Int ->
  MenuSem r i ()
cycleMenu offset = do
  unSemS do
    count <- use (to numVisible)
    cursor %= \ currentCount -> fromMaybe 0 ((currentCount + fromIntegral offset) `mod` fromIntegral count)

menuModify ::
  Members [Resource, Embed IO] r =>
  MenuSem r i () ->
  MenuWidgetSem r i a
menuModify action = do
  menuWriteSem action
  menuRender

menuNavigate ::
  Members [Resource, Embed IO] r =>
  MenuSem r i () ->
  MenuWidgetSem r i a
menuNavigate action = do
  menuRead action
  menuRender

menuCycle ::
  Members [Resource, Embed IO] r =>
  Int ->
  MenuWidgetSem r i a
menuCycle offset =
  menuNavigate (cycleMenu offset)

toggleSelected ::
  MenuSem r i ()
toggleSelected = do
  semState do
    CursorIndex cur <- use cursor
    entries %= overEntries \ index ->
      if index == cur
      then Entry.selected %~ not
      else id
    SemS (cycleMenu 1)

menuToggle ::
  Members [Resource, Embed IO] r =>
  MenuWidgetSem r i a
menuToggle =
  menuModify toggleSelected

menuToggleAll ::
  Members [Resource, Embed IO] r =>
  MenuWidgetSem r i a
menuToggleAll =
  menuModify $ semState do
    entries %= overEntries (const (Entry.selected %~ not))

menuUpdatePrompt ::
  Prompt ->
  MenuWidgetSem r i a
menuUpdatePrompt prompt =
  act (MenuAction.UpdatePrompt prompt)
