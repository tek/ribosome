module Ribosome.Menu.Data.RenderMenu where

import Ribosome.Menu.Class.MenuMode (MenuMode (renderExtra, renderFilter))
import Ribosome.Menu.Class.MenuState (MenuState (Item, core, mode, renderStatus))
import Ribosome.Menu.Data.CursorIndex (CursorIndex (CursorIndex))
import Ribosome.Menu.Data.Entry (Entries)
import Ribosome.Menu.Data.MenuAction (RenderAnchor)
import Ribosome.Menu.Data.MenuStatus (MenuStatus (MenuStatus))
import qualified Ribosome.Menu.Data.State as Modal
import Ribosome.Menu.Data.State (Core (Core), Primary (Primary))
import Ribosome.Menu.Data.WithCursor (WithCursor (WithCursor))

data RenderMenu i =
  RenderMenu {
    entries :: Entries i,
    cursor :: CursorIndex,
    status :: MenuStatus,
    anchor :: RenderAnchor
  }
  deriving stock (Generic)

menuStatus ::
  ∀ s .
  MenuState s =>
  Word ->
  Word ->
  CursorIndex ->
  s ->
  MenuStatus
menuStatus itemCount entryCount (CursorIndex cursor) s =
  MenuStatus (renderFilter @(Item s) md) (renderExtra @(Item s) md) (renderStatus s) itemCount entryCount cursor
  where
    md = s ^. mode

fromState ::
  ∀ s .
  MenuState s =>
  WithCursor s ->
  RenderAnchor ->
  RenderMenu (Item s)
fromState (WithCursor s cursor) anchor =
  RenderMenu {status = menuStatus @s itemCount entryCount cursor s, ..}
  where
    Core {primary = Primary {entries}, itemCount, entryCount} =
      s ^. core
