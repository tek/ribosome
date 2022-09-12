module Ribosome.Menu.Data.Entry where

import qualified Data.IntMap.Strict as IntMap
import Data.Semigroup (Sum (Sum, getSum))

import Ribosome.Host.Data.Tuple (dup)
import Ribosome.Menu.Data.MenuItem (MenuItem (render), simpleMenuItem)

data Entry a =
  Entry {
    item :: MenuItem a,
    index :: Int,
    selected :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance Eq a => Ord (Entry a) where
  compare =
    comparing index <> comparing (render . item)

type Entries a =
  IntMap (Seq (Entry a))

tuple :: Entry a -> (Int, MenuItem a)
tuple Entry {..} =
  (index, item)

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
  intEntries . fmap dup

entriesLength :: Entries a -> Int
entriesLength =
  getSum . foldMap (Sum . length)
