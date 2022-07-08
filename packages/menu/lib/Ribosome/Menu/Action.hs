module Ribosome.Menu.Action where

import Data.Generics.Labels ()
import Lens.Micro.Mtl (use, (%=))

import Ribosome.Menu.Combinators (numVisible, overEntries)
import Ribosome.Menu.Data.CursorIndex (CursorIndex (CursorIndex))
import qualified Ribosome.Menu.Data.MenuAction as MenuAction
import Ribosome.Menu.Data.MenuAction (MenuAction)
import Ribosome.Menu.Data.MenuState (MenuWidget, ModifyCursor, ModifyMenu, liftCursor, modifyCursor, modifyMenu, semState)
import Ribosome.Menu.Effect.MenuState (MenuState)
import Ribosome.Menu.ItemLens (entries)
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
  ModifyCursor i r ()
cycleMenu offset = do
  count <- asks numVisible
  modify' \ currentCount -> fromMaybe 0 ((currentCount + fromIntegral offset) `mod` fromIntegral count)

menuModify ::
  Member (MenuState i) r =>
  ModifyMenu i r () ->
  MenuWidget r a
menuModify action = do
  modifyMenu action
  menuRender

menuNavigate ::
  Member (MenuState i) r =>
  ModifyCursor i r () ->
  MenuWidget r a
menuNavigate action = do
  modifyCursor action
  menuRender

menuCycle ::
  Member (MenuState i) r =>
  Int ->
  MenuWidget r a
menuCycle offset =
  menuNavigate (cycleMenu offset)

toggleSelected ::
  ModifyMenu i r ()
toggleSelected = do
  semState do
    CursorIndex cur <- use #cursor
    entries %= overEntries \ index ->
      if index == cur
      then #selected %~ not
      else id
  liftCursor (cycleMenu 1)

menuToggle ::
  Member (MenuState i) r =>
  MenuWidget r a
menuToggle =
  menuModify toggleSelected

menuToggleAll ::
  Member (MenuState i) r =>
  MenuWidget r a
menuToggleAll =
  menuModify $ semState do
    entries %= overEntries (const (#selected %~ not))

menuUpdatePrompt ::
  Prompt ->
  MenuWidget r a
menuUpdatePrompt prompt =
  act (MenuAction.UpdatePrompt prompt)
