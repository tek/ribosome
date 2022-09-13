module Ribosome.Menu.Data.RenderMenu where

import Ribosome.Menu.Class.MenuMode (MenuMode (renderExtra, renderFilter))
import Ribosome.Menu.Class.MenuState (MenuState (Item, core, mode))
import Ribosome.Menu.Data.CursorIndex (CursorIndex (CursorIndex))
import Ribosome.Menu.Data.Entry (Entries)
import Ribosome.Menu.Data.MenuStatus (MenuStatus (MenuStatus))
import qualified Ribosome.Menu.Data.State as Modal
import Ribosome.Menu.Data.State (Core (Core))
import Ribosome.Menu.Data.WithCursor (WithCursor (WithCursor))

data RenderMenu i =
  RenderMenu {
    entries :: Entries i,
    cursor :: CursorIndex,
    status :: MenuStatus
  }
  deriving stock (Eq, Show, Generic)

menuStatus ::
  ∀ i mode .
  MenuMode i mode =>
  Int ->
  Int ->
  CursorIndex ->
  mode ->
  MenuStatus
menuStatus itemCount entryCount (CursorIndex cursor) md =
  MenuStatus (renderFilter @i md) (renderExtra @i md) itemCount entryCount cursor

fromState ::
  ∀ s .
  MenuState s =>
  WithCursor s ->
  RenderMenu (Item s)
fromState (WithCursor s cursor) =
  RenderMenu {status = menuStatus @(Item s) itemCount entryCount cursor (s ^. mode), ..}
  where
    Core {entries, itemCount, entryCount} =
      s ^. core
