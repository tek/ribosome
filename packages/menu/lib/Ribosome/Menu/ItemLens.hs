module Ribosome.Menu.ItemLens where

import qualified Data.IntMap.Strict as IntMap
import Lens.Micro.Mtl (view)

import Ribosome.Menu.Class.MenuState (MenuState (Item), entries)
import Ribosome.Menu.Combinators (filterEntries, foldEntries, sortedEntries)
import Ribosome.Menu.Data.CursorIndex (CursorIndex (CursorIndex))
import qualified Ribosome.Menu.Data.Entry as Entry
import Ribosome.Menu.Data.Entry (Entry)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.WithCursor (WithCursor)

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
  MenuState s =>
  [Int] ->
  s ->
  [Entry (Item s)]
entriesByIndex indexes s =
  filterIndexesFlat indexes (s ^. sortedEntries)

itemsByEntryIndex ::
  MenuState s =>
  [Int] ->
  s ->
  Maybe (NonEmpty (MenuItem (Item s)))
itemsByEntryIndex indexes menu =
  nonEmpty (Entry.item <$> entriesByIndex indexes menu)

getFocus ::
  MenuState s =>
  WithCursor s ->
  Maybe (MenuItem (Item s))
getFocus s =
  s ^? sortedEntries . ix (fromIntegral (s ^. #cursor)) . #item

focus ::
  MenuState s =>
  SimpleGetter (WithCursor s) (Maybe (MenuItem (Item s)))
focus =
  to getFocus

selectedItemsOnly ::
  MenuState s =>
  s ->
  Maybe (NonEmpty (MenuItem (Item s)))
selectedItemsOnly =
  nonEmpty . fmap Entry.item . view (entries . to (filterEntries (const Entry.selected)))

selectedOnly ::
  MenuState s =>
  SimpleGetter s (Maybe (NonEmpty (MenuItem (Item s))))
selectedOnly =
  to selectedItemsOnly

selectedItems ::
  MenuState s =>
  WithCursor s ->
  Maybe (NonEmpty (MenuItem (Item s)))
selectedItems s =
  selectedItemsOnly s <|> (pure <$> getFocus s)

selected' ::
  MenuState s =>
  SimpleGetter (WithCursor s) (Maybe (NonEmpty (MenuItem (Item s))))
selected' =
  to selectedItems

selected ::
  MenuState s =>
  SimpleGetter (WithCursor s) (Maybe (NonEmpty (Item s)))
selected =
  to (fmap (fmap MenuItem.meta) . selectedItems)

menuItemsByIndexes ::
  MenuState s =>
  [Int] ->
  s ->
  [MenuItem (Item s)]
menuItemsByIndexes indexes =
  maybe [] toList . itemsByEntryIndex indexes

-- |Extract either the entries that aren't selected, or, if none are, all but the focused entry.
-- This needs two passes since the result should be sorted and it's impossible to know whether the focus should be
-- included or not before the entire dataset was traversed.
unselectedItems ::
  MenuState s =>
  WithCursor s ->
  [MenuItem (Item s)]
unselectedItems s =
  Entry.item <$> view (entries . to filterUnselected) s
  where
    filterUnselected =
      uncurry extract .
      first toList .
      foldEntries folder (mempty, False)
    extract ents = \case
      True -> mapMaybe rightToMaybe ents
      False -> either id id <$> ents
    folder (z, _) _ e | Entry.selected e =
      (z, True)
    folder (z, foundSelected) i e | i == cursorIndex =
      (Left e : z, foundSelected)
    folder (z, _) _ e =
      (Right e : z, True)
    CursorIndex cursorIndex =
      s ^. #cursor

unselected ::
  MenuState s =>
  SimpleGetter (WithCursor s) [MenuItem (Item s)]
unselected =
  to unselectedItems
