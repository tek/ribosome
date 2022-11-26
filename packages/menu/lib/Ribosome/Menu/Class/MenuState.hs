module Ribosome.Menu.Class.MenuState where

import Data.Trie (Trie)

import qualified Ribosome.Menu.Class.MenuMode as MenuMode
import Ribosome.Menu.Class.MenuMode (MenuMode)
import Ribosome.Menu.Data.Entry (Entries)
import Ribosome.Menu.Data.MenuItem (Items)
import Ribosome.Menu.Data.State (Core, MenuQuery, Modal)
import Ribosome.Menu.Data.WithCursor (WithCursor)

class (
    MenuMode (Item s) (Mode s)
  ) => MenuState s where
  type Item s :: Type
  type Mode s :: Type

  core :: Lens' s (Core (Item s))

  mode :: Lens' s (Mode s)

  histories :: Lens' s (Map (Mode s) (Trie (Entries (Item s))))

  renderStatus :: s -> Int -> [Text]
  renderStatus _ _ = []

type Filter s =
  MenuMode.Filter (Mode s)

instance (
    MenuMode i m
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
  Traversal' s (Trie (Entries (Item s)))
history m =
  histories . ix m

items ::
  MenuState s =>
  Lens s s (Items (Item s)) (Items (Item s))
items =
  core . #items

entries ::
  MenuState s =>
  Lens s s (Entries (Item s)) (Entries (Item s))
entries =
  core . #entries

itemCount ::
  MenuState s =>
  Lens s s Int Int
itemCount =
  core . #itemCount

entryCount ::
  MenuState s =>
  Lens s s Int Int
entryCount =
  core . #entryCount

query ::
  MenuState s =>
  Lens s s MenuQuery MenuQuery
query =
  core . #query

filterMode ::
  MenuState s =>
  SimpleGetter s (Filter s (Item s))
filterMode =
  mode . to MenuMode.filterMode
