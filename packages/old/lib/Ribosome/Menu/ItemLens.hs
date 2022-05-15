{-# options_ghc -Wno-redundant-constraints #-}

module Ribosome.Menu.ItemLens where

import Control.Lens (Getter, element, to, views, (^?))
import qualified Data.IntMap.Strict as IntMap

import Ribosome.Menu.Combinators (filterEntries, sortedEntries, foldEntries)
import qualified Ribosome.Menu.Data.Entry as Entry
import Ribosome.Menu.Data.Entry (Entry)
import Ribosome.Menu.Data.MenuData (HasMenuCursor, HasMenuItems, cursor, entries)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.CursorIndex (CursorIndex(CursorIndex))

filterIndexesFlat :: [Int] -> [a] -> [a]
filterIndexesFlat indexes =
  reverse . go 0 (sort indexes) []
  where
    go cur (i : is) result (a : asTail) | i == cur =
      go (cur + 1) is (a : result) asTail
    go cur is result (_ : asTail) =
      go (cur + 1) is result asTail
    go _ _ result _ =
      result

partitionIndexesFlat :: [Int] -> [a] -> ([a], [a])
partitionIndexesFlat indexes =
  bimap reverse reverse . go 0 (sort indexes) ([], [])
  where
    go cur (i : is) (yes, no) (a : asTail) | i == cur =
      go (cur + 1) is (a : yes, no) asTail
    go cur is (yes, no) (a : asTail) =
      go (cur + 1) is (yes, a : no) asTail
    go _ _ result [] =
      result

filterIndexes :: IntSet -> IntMap a -> IntMap a
filterIndexes indexes m =
  IntMap.restrictKeys m indexes

entriesByIndex ::
  HasMenuCursor a =>
  HasMenuItems a i =>
  [Int] ->
  a ->
  [Entry i]
entriesByIndex indexes menu =
  filterIndexesFlat indexes (menu ^. sortedEntries)

itemsByEntryIndex ::
  HasMenuCursor a =>
  HasMenuItems a i =>
  [Int] ->
  a ->
  Maybe (NonEmpty (MenuItem i))
itemsByEntryIndex indexes menu =
  nonEmpty (Entry._item <$> entriesByIndex indexes menu)

getFocus ::
  HasMenuCursor a =>
  HasMenuItems a i =>
  a ->
  Maybe (MenuItem i)
getFocus menu =
  menu ^? sortedEntries . element (fromIntegral (menu ^. cursor)) . Entry.item

focus ::
  HasMenuCursor a =>
  HasMenuItems a i =>
  Getter a (Maybe (MenuItem i))
focus =
  to getFocus

selectedItemsOnly ::
  HasMenuItems a i =>
  a ->
  Maybe (NonEmpty (MenuItem i))
selectedItemsOnly =
  nonEmpty . fmap Entry._item . views entries (filterEntries \ _ -> Entry._selected)

selectedOnly ::
  HasMenuItems a i =>
  Getter a (Maybe (NonEmpty (MenuItem i)))
selectedOnly =
  to selectedItemsOnly

selectedItems ::
  HasMenuCursor a =>
  HasMenuItems a i =>
  a ->
  Maybe (NonEmpty (MenuItem i))
selectedItems m =
  selectedItemsOnly m <|> (pure <$> getFocus m)

selected' ::
  HasMenuCursor a =>
  HasMenuItems a i =>
  Getter a (Maybe (NonEmpty (MenuItem i)))
selected' =
  to selectedItems

selected ::
  HasMenuCursor a =>
  HasMenuItems a i =>
  Getter a (Maybe (NonEmpty i))
selected =
  to (fmap (fmap MenuItem._meta) . selectedItems)

menuItemsByIndexes ::
  HasMenuCursor a =>
  HasMenuItems a i =>
  [Int] ->
  a ->
  [MenuItem i]
menuItemsByIndexes indexes =
  maybe [] toList . itemsByEntryIndex indexes

-- |Extract either the entries that aren't selected, or, if none are, all but the focused entry.
-- This needs two passes since the result should be sorted and it's impossible to know whether the focus should be
-- included or not before the entire dataset was traversed.
unselectedItems ::
  HasMenuCursor a =>
  HasMenuItems a i =>
  a ->
  [MenuItem i]
unselectedItems menu =
  Entry._item <$> views entries filterUnselected menu
  where
    filterUnselected =
      uncurry extract .
      first toList .
      foldEntries folder (mempty, False)
    extract ents = \case
      True -> mapMaybe rightToMaybe ents
      False -> either id id <$> ents
    folder (z, _) _ e | Entry._selected e =
      (z, True)
    folder (z, foundSelected) i e | i == cursorIndex =
      (Left e : z, foundSelected)
    folder (z, _) _ e =
      (Right e : z, True)
    CursorIndex cursorIndex =
      menu ^. cursor

unselected ::
  HasMenuCursor a =>
  HasMenuItems a i =>
  Getter a [MenuItem i]
unselected =
  to unselectedItems
