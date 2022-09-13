module Ribosome.Menu.Data.Menu where

-- import Ribosome.Menu.Data.CursorIndex (CursorIndex)
-- import Ribosome.Menu.Data.State (Modal)

-- data Menu filter i =
--   Menu {
--     items :: Modal filter i,
--     cursor :: CursorIndex
--   }
--   deriving stock (Eq, Show, Generic)
--   deriving anyclass (Default)

-- consMenu ::
--   Items i ->
--   Entries i ->
--   Map (mode i) (Trie (Entries i)) ->
--   Int ->
--   Int ->
--   MenuQuery ->
--   filter ->
--   CursorIndex ->
--   Menu mode i
-- consMenu it en hist cnt ecnt curr currF curs =
--   Menu (Modal it en hist cnt ecnt curr currF) curs
