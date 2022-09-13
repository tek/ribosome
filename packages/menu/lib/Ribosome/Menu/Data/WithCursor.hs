module Ribosome.Menu.Data.WithCursor where

import Ribosome.Menu.Data.CursorIndex (CursorIndex)

data WithCursor s =
  WithCursor {
    state :: s,
    cursor :: CursorIndex
  }
  deriving stock (Eq, Show, Generic)
