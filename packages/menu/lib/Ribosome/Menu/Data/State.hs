module Ribosome.Menu.Data.State where

import qualified Data.Map.Strict as Map
import Data.Trie (Trie)

import Ribosome.Menu.Data.Entry (Entries)
import qualified Ribosome.Menu.Data.Filter as Filter
import Ribosome.Menu.Data.MenuItem (Items)

newtype MenuQuery =
  MenuQuery { unMenuQuery :: Text }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid, Ord, IsString)

data Core i =
  Core {
    items :: Items i,
    entries :: Entries i,
    itemCount :: Word,
    entryCount :: Word,
    query :: MenuQuery
  }
  deriving stock (Eq, Show, Generic)

data Modal mode i =
  Modal {
    core :: Core i,
    history :: Map mode (Trie (Entries i)),
    mode :: mode
  }
  deriving stock (Eq, Show, Generic)

type ModalState i =
  Modal Filter.Filter i

modal ::
  mode ->
  Modal mode i
modal =
  Modal (Core mempty mempty 0 0 mempty) Map.empty

instance (
    Default mode
  ) => Default (Modal mode i) where
    def =
      modal def
