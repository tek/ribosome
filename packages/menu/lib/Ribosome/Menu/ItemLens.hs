module Ribosome.Menu.ItemLens where

import qualified Data.IntMap.Strict as IntMap
import Data.Trie (Trie)
import Lens.Micro.Mtl (view)

import Ribosome.Menu.Combinators (filterEntries, foldEntries, sortedEntries)
import Ribosome.Menu.Data.CursorIndex (CursorIndex (CursorIndex))
import qualified Ribosome.Menu.Data.Entry as Entry
import Ribosome.Menu.Data.Entry (Entries, Entry)
import Ribosome.Menu.Data.Menu (Menu)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Data.MenuItem (Items, MenuItem)
import Ribosome.Menu.Data.MenuItems (MenuQuery)

entries :: Lens' (Menu i) (Entries i)
entries =
  #items . #entries

history :: Lens' (Menu i) (Trie (Entries i))
history =
  #items . #history

items :: Lens' (Menu i) (Items i)
items =
  #items . #items

currentQuery :: Lens' (Menu i) MenuQuery
currentQuery =
  #items . #currentQuery

itemCount :: Lens' (Menu i) Int
itemCount =
  #items . #itemCount

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
  [Int] ->
  Menu i ->
  [Entry i]
entriesByIndex indexes menu =
  filterIndexesFlat indexes (menu ^. #items . sortedEntries)

itemsByEntryIndex ::
  [Int] ->
  Menu i ->
  Maybe (NonEmpty (MenuItem i))
itemsByEntryIndex indexes menu =
  nonEmpty (Entry.item <$> entriesByIndex indexes menu)

getFocus ::
  Menu i ->
  Maybe (MenuItem i)
getFocus menu =
  menu ^? #items . sortedEntries . ix (fromIntegral (menu ^. #cursor)) . #item

focus ::
  SimpleGetter (Menu i) (Maybe (MenuItem i))
focus =
  to getFocus

selectedItemsOnly ::
  Menu i ->
  Maybe (NonEmpty (MenuItem i))
selectedItemsOnly =
  nonEmpty . fmap Entry.item . view (#items . #entries . to (filterEntries (const Entry.selected)))

selectedOnly ::
  SimpleGetter (Menu i) (Maybe (NonEmpty (MenuItem i)))
selectedOnly =
  to selectedItemsOnly

selectedItems ::
  Menu i ->
  Maybe (NonEmpty (MenuItem i))
selectedItems m =
  selectedItemsOnly m <|> (pure <$> getFocus m)

selected' ::
  SimpleGetter (Menu i) (Maybe (NonEmpty (MenuItem i)))
selected' =
  to selectedItems

selected ::
  SimpleGetter (Menu i) (Maybe (NonEmpty i))
selected =
  to (fmap (fmap MenuItem.meta) . selectedItems)

menuItemsByIndexes ::
  [Int] ->
  Menu i ->
  [MenuItem i]
menuItemsByIndexes indexes =
  maybe [] toList . itemsByEntryIndex indexes

-- |Extract either the entries that aren't selected, or, if none are, all but the focused entry.
-- This needs two passes since the result should be sorted and it's impossible to know whether the focus should be
-- included or not before the entire dataset was traversed.
unselectedItems ::
  Menu i ->
  [MenuItem i]
unselectedItems menu =
  Entry.item <$> view (#items . #entries . to filterUnselected) menu
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
      menu ^. #cursor

unselected ::
  SimpleGetter (Menu i) [MenuItem i]
unselected =
  to unselectedItems
