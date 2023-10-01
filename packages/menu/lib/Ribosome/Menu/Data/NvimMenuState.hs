module Ribosome.Menu.Data.NvimMenuState where

import qualified Ribosome.Menu.Data.Entry
import Ribosome.Menu.Data.Entry (Entry, ItemIndex)
import qualified Ribosome.Menu.Data.MenuItem
import Ribosome.Menu.Data.MenuView (EntryIndex)

data PartialEntry a =
  PartialEntry {
    entry :: Entry a,
    visibleLines :: Word
  }
  deriving stock (Eq, Show, Generic, Functor)

partialLength :: Maybe (PartialEntry a) -> Word
partialLength = \case
  Just e -> e.visibleLines
  Nothing -> 0

data EntrySlice i =
  EntrySlice {
    full :: [Entry i],
    indexBot :: EntryIndex,
    indexTop :: EntryIndex,
    partialBot :: Maybe (PartialEntry i),
    partialTop :: Maybe (PartialEntry i)
  }
  |
  OnlyPartialEntry {
    entry :: PartialEntry i,
    index :: EntryIndex
  }
  deriving stock (Eq, Show, Generic, Functor)

sliceRange :: EntrySlice i -> (EntryIndex, EntryIndex)
sliceRange = \case
  EntrySlice {..} -> (indexBot, indexTop)
  OnlyPartialEntry {index} -> (index, index)

sliceLength :: EntrySlice i -> Word
sliceLength = \case
  EntrySlice {full, partialBot, partialTop} ->
    sum (full <&> \ e -> e.item.lines) + partialLength partialBot + partialLength partialTop
  OnlyPartialEntry {entry} -> entry.visibleLines

data SliceIndexes =
  SliceIndexes {
    full :: [(ItemIndex, Bool)],
    partialBot :: Maybe (ItemIndex, Word, Bool),
    partialTop :: Maybe (ItemIndex, Word, Bool)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)
