module Ribosome.Menu.Action where

import Control.Lens (to, use, (%=))

import Ribosome.Menu.Combinators (numVisible, overEntries)
import Ribosome.Menu.Data.CursorIndex (CursorIndex (CursorIndex))
import qualified Ribosome.Menu.Data.Entry as Entry
import qualified Ribosome.Menu.Data.MenuAction as MenuAction
import Ribosome.Menu.Data.MenuAction (MenuAction)
import Ribosome.Menu.Data.MenuConsumer (MenuWidget)
import Ribosome.Menu.Data.MenuData (cursor, entries)
import Ribosome.Menu.Data.MenuState (MenuM, menuRead, menuWrite)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

act ::
  Applicative m =>
  MenuAction m1 a ->
  m (Maybe (MenuAction m1 a))
act =
  pure . Just

menuIgnore ::
  Applicative m =>
  m (Maybe (MenuAction m1 a))
menuIgnore =
  pure Nothing

menuOk ::
  Applicative m =>
  m (Maybe (MenuAction m1 a))
menuOk =
  pure (Just MenuAction.Continue)

menuRender ::
  Applicative m =>
  m (Maybe (MenuAction m1 a))
menuRender =
  act MenuAction.Render

menuQuit ::
  Applicative m =>
  Applicative m1 =>
  m (Maybe (MenuAction m1 a))
menuQuit =
  act MenuAction.abort

menuSuccess ::
  Applicative m =>
  Applicative m1 =>
  m1 a ->
  m (Maybe (MenuAction m1 a))
menuSuccess ma =
  act (MenuAction.success ma)

menuResult ::
  Applicative m =>
  Applicative m1 =>
  a ->
  m (Maybe (MenuAction m1 a))
menuResult =
  menuSuccess . pure

cycleMenu ::
  Monad m =>
  Int ->
  MenuM m i ()
cycleMenu offset = do
  count <- use (to numVisible)
  cursor %= \ currentCount -> if count == 0 then 0 else (currentCount + fromIntegral offset) `mod` fromIntegral count

menuModify ::
  MonadIO m =>
  MonadBaseControl IO m =>
  MenuM m i () ->
  MenuWidget m i a
menuModify action = do
  menuWrite action
  menuRender

menuNavigate ::
  MonadIO m =>
  MonadBaseControl IO m =>
  MenuM m i () ->
  MenuWidget m i a
menuNavigate action = do
  menuRead action
  menuRender

menuCycle ::
  MonadIO m =>
  MonadBaseControl IO m =>
  Int ->
  MenuWidget m i a
menuCycle offset =
  menuNavigate (cycleMenu offset)

toggleSelected ::
  Monad m =>
  MenuM m i ()
toggleSelected = do
  CursorIndex cur <- use cursor
  entries %= overEntries \ index ->
    if index == cur
    then Entry.selected %~ not
    else id
  cycleMenu 1

menuToggle ::
  MonadIO m =>
  MonadBaseControl IO m =>
  MenuWidget m i a
menuToggle =
  menuModify toggleSelected

menuToggleAll ::
  MonadIO m =>
  MonadBaseControl IO m =>
  MenuWidget m i a
menuToggleAll =
  menuModify $ entries %= overEntries (const (Entry.selected %~ not))

menuUpdatePrompt ::
  Monad (t m) =>
  Prompt ->
  t m (Maybe (MenuAction m a))
menuUpdatePrompt prompt =
  act (MenuAction.UpdatePrompt prompt)
