module Ribosome.Menu.Data.Entry where

import Control.Lens (makeClassy)
import qualified Data.IntMap.Strict as IntMap

import Ribosome.Menu.Data.MenuItem (HasMenuItem (menuItem), MenuItem (_truncated), simpleMenuItem)

data Entry a =
  Entry {
    _item :: MenuItem a,
    _index :: Int,
    _selected :: Bool
  }
  deriving stock (Eq, Show)

makeClassy ''Entry

instance Eq a => Ord (Entry a) where
  compare =
    comparing _index <> comparing (_truncated . _item)

instance HasMenuItem (Entry a) a where
  menuItem =
    item

type Entries a =
  IntMap (Seq (Entry a))

tuple :: Entry a -> (Int, MenuItem a)
tuple Entry {..} =
  (_index, _item)

insertFiltered :: Int -> Entry a -> Entries a -> Entries a
insertFiltered i it =
  IntMap.insertWith (flip (<>)) i (pure it)

fromList :: [(Int, Entry a)] -> Entries a
fromList =
  IntMap.fromListWith (flip (<>)) . fmap (second pure)

intEntries :: [(Int, Int)] -> Entries Int
intEntries nums =
  fromList [(score, Entry (simpleMenuItem i (show i)) i False) | (score, i) <- nums]

simpleIntEntries :: [Int] -> Entries Int
simpleIntEntries =
  intEntries . fmap \ a -> (a, a)
