module Ribosome.Menu.Data.Entry where

import qualified Data.IntMap.Strict as IntMap
import Data.Semigroup (Sum (Sum, getSum))

import qualified Ribosome.Menu.Data.MenuItem
import Ribosome.Menu.Data.MenuItem (MenuItem, simpleMenuItem, simpleMenuItemLines)

newtype ItemIndex =
  ItemIndex { value :: Word }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

data Entry a =
  Entry {
    item :: MenuItem a,
    index :: ItemIndex,
    selected :: Bool
  }
  deriving stock (Eq, Show, Generic, Functor)

instance Eq a => Ord (Entry a) where
  compare =
    comparing (.index) <> comparing (.item.render)

type Entries a =
  IntMap (Seq (Entry a))

fromList :: [(Int, Entry a)] -> Entries a
fromList =
  IntMap.fromListWith (flip (<>)) . fmap (second pure)

intEntries :: [(Int, Word)] -> Entries Word
intEntries nums =
  fromList [(score, Entry (simpleMenuItem i (show i)) (ItemIndex i) False) | (score, i) <- nums]

multi :: Word -> NonEmpty Text -> Entry Word
multi i ls =
  Entry (simpleMenuItemLines i ls) (ItemIndex i) False

multis :: [(Int, Word, NonEmpty Text)] -> Entries Word
multis es =
  fromList [(score, multi i ls) | (score, i, ls) <- es]

simpleIntEntries :: [Word] -> Entries Word
simpleIntEntries =
  intEntries . fmap \ i -> (fromIntegral i, i)

entriesLength :: Entries a -> Word
entriesLength =
  fromIntegral . getSum . foldMap (Sum . length)

entryLineCount :: Entry a -> Word
entryLineCount e = fromIntegral (length e.item.render)

-- | Calculate the total number of lines in an 'Entry' list.
entriesLineCount ::
  Foldable t =>
  t (Entry a) ->
  Word
entriesLineCount =
  getSum . foldMap (Sum . entryLineCount)
