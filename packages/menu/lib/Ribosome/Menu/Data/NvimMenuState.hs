module Ribosome.Menu.Data.NvimMenuState where

import Ribosome.Menu.Data.Entry (Entry)
import Ribosome.Menu.Data.MenuView (EntryIndex, MenuView)

data PartialEntry a =
  PartialEntry {
    entry :: Entry a,
    visibleLines :: Word
  }
  deriving stock (Eq, Show, Generic)

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
  deriving stock (Eq, Show, Generic)

sliceRange :: EntrySlice i -> (EntryIndex, EntryIndex)
sliceRange = \case
  EntrySlice {..} -> (indexBot, indexTop)
  OnlyPartialEntry {index} -> (index, index)

data SliceIndexes =
  SliceIndexes {
    full :: [(Word, Bool)],
    partialBot :: Maybe (Word, Word, Bool),
    partialTop :: Maybe (Word, Word, Bool)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)

data NvimMenuState =
  NvimMenuState {
    view :: MenuView,
    slice :: SliceIndexes
  }
  deriving stock (Eq, Show, Generic)

instance Default NvimMenuState where
  def = NvimMenuState def def
