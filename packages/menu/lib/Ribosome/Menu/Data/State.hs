module Ribosome.Menu.Data.State where

import qualified Data.Map.Strict as Map
import Data.Trie (Trie)

import Ribosome.Menu.Data.Entry (Entries)
import Ribosome.Menu.Data.Filter (Filter)
import Ribosome.Menu.Data.MenuItem (Items)
import Ribosome.Menu.Data.MenuQuery (MenuQuery)

data Primary i =
  Primary {
    items :: Items i,
    entries :: Entries i,
    query :: MenuQuery
  }
  deriving stock (Eq, Show, Generic)

data Core i =
  Core {
    primary :: Primary i,
    itemCount :: Word,
    entryCount :: Word
  }
  deriving stock (Eq, Show, Generic)

data Modal mode i =
  Modal {
    core :: Core i,
    history :: Map mode (Trie (Entries i)),
    mode :: mode
  }
  deriving stock (Eq, Show, Generic)

type ModalState i = Modal Filter i

modal ::
  ∀ i mode .
  mode ->
  Modal mode i
modal =
  Modal (Core (Primary mempty mempty mempty) 0 0) Map.empty

instance Default mode => Default (Modal mode i) where
  def = modal def
