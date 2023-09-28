module Ribosome.Menu.Class.MenuState where

import Data.Trie (Trie)

import Ribosome.Menu.Class.MenuMode (MenuMode)
import Ribosome.Menu.Data.Entry (Entries)
import Ribosome.Menu.Data.MenuItem (Items)
import Ribosome.Menu.Data.MenuQuery (MenuQuery)
import Ribosome.Menu.Data.State (Core, Modal)
import Ribosome.Menu.Data.WithCursor (WithCursor)

class (
    MenuMode (Item s) (Mode s),
    Show (Item s)
  ) => MenuState s where
  type Item s :: Type
  type Mode s :: Type

  core :: Lens' s (Core (Item s))

  mode :: Lens' s (Mode s)

  histories :: Lens' s (Map (Mode s) (Trie (Entries (Item s))))

  renderStatus :: s -> Int -> [Text]
  renderStatus _ _ = []

instance (
    MenuMode i m,
    Show (Item (Modal m i))
  ) => MenuState (Modal m i) where
    type Item (Modal m i) = i
    type Mode (Modal m i) = m

    core = #core

    mode = #mode

    histories = #history

instance (
    MenuState s
  ) => MenuState (WithCursor s) where
    type Item (WithCursor s) = Item s
    type Mode (WithCursor s) = Mode s

    core = #state . core

    mode = #state . mode

    histories = #state . histories

history ::
  MenuState s =>
  Mode s ->
  Traversal' s (Maybe (Trie (Entries (Item s))))
history m =
  histories . at m

items ::
  MenuState s =>
  Lens s s (Items (Item s)) (Items (Item s))
items =
  core . #primary . #items

entries ::
  MenuState s =>
  Lens s s (Entries (Item s)) (Entries (Item s))
entries =
  core . #primary . #entries

itemCount ::
  MenuState s =>
  Lens s s Word Word
itemCount =
  core . #itemCount

entryCount ::
  MenuState s =>
  Lens s s Word Word
entryCount =
  core . #entryCount

query ::
  MenuState s =>
  Lens' s MenuQuery
query =
  core . #primary . #query
