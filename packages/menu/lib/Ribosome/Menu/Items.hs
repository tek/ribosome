-- | Combinators for running actions on the selected or focused menu items.
-- Intended to be used from mapping handlers.
-- 'withFocus' and 'withSelection' will skip processing of the event downstream if the menu is empty.
module Ribosome.Menu.Items where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import Prelude hiding (unify)

import Ribosome.Menu.Class.MenuState (MenuState (Item, histories, mode), entries, history, items)
import Ribosome.Menu.Combinators (overEntry, sortedEntries)
import Ribosome.Menu.Data.CursorIndex (CursorIndex (CursorIndex))
import qualified Ribosome.Menu.Data.Entry
import Ribosome.Menu.Data.Entry (Entries, Entry)
import qualified Ribosome.Menu.Data.MenuAction as MenuAction
import Ribosome.Menu.Data.MenuAction (MenuAction)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.WithCursor (WithCursor)
import Ribosome.Menu.Effect.Menu (Menu, menuState, readState, viewMenu)
import Ribosome.Menu.ItemLens (focus, focusEntry, selected, selected')
import Ribosome.Menu.Lens (use, (%=), (.=))

-- | Run an action with the focused entry if the menu is non-empty.
withFocusEntry' ::
  MenuState s =>
  Member (Menu s) r =>
  (Entry (Item s) -> Sem r (Maybe a)) ->
  Sem r (Maybe a)
withFocusEntry' f =
  fmap join . traverse f =<< viewMenu focusEntry

-- | Run an action with the focused entry and quit the menu with the returned value.
-- If the menu was empty, do nothing (i.e. skip the event).
withFocusEntry ::
  MenuState s =>
  Member (Menu s) r =>
  (Entry (Item s) -> Sem r a) ->
  Sem r (Maybe (MenuAction a))
withFocusEntry f =
  Just . maybe MenuAction.Continue MenuAction.success <$> withFocusEntry' (fmap Just . f)

-- | Run an action with the focused entry's item if the menu is non-empty.
withFocusItem ::
  MenuState s =>
  Member (Menu s) r =>
  (MenuItem (Item s) -> Sem r a) ->
  Sem r (Maybe a)
withFocusItem f =
  traverse f =<< viewMenu focus

-- | Run an action with the focused entry's item if the menu is non-empty, extracting the item payload.
withFocus' ::
  MenuState s =>
  Member (Menu s) r =>
  (Item s -> Sem r (Maybe a)) ->
  Sem r (Maybe a)
withFocus' f =
  join <$> withFocusItem (f . (.meta))

-- | Run an action with the focused entry's item and quit the menu with the returned value.
-- If the menu was empty, do nothing (i.e. skip the event).
withFocus ::
  MenuState s =>
  Member (Menu s) r =>
  (Item s -> Sem r a) ->
  Sem r (Maybe (MenuAction a))
withFocus f =
  Just . maybe MenuAction.Continue MenuAction.success <$> withFocus' (fmap Just . f)

-- | Run an action with the selection or the focused entry if the menu is non-empty.
withSelectionItems ::
  MenuState s =>
  Member (Menu s) r =>
  (NonEmpty (MenuItem (Item s)) -> Sem r a) ->
  Sem r (Maybe a)
withSelectionItems f =
  traverse f =<< viewMenu selected'

-- | Run an action with the selection or the focused entry if the menu is non-empty, extracting the item payloads.
withSelection' ::
  MenuState s =>
  Member (Menu s) r =>
  (NonEmpty (Item s) -> Sem r (Maybe a)) ->
  Sem r (Maybe a)
withSelection' f =
  fmap join . traverse f =<< viewMenu selected

-- | Run an action with the selection or the focused entry and quit the menu with the returned value.
-- If the menu was empty, do nothing (i.e. skip the event).
withSelection ::
  MenuState s =>
  Member (Menu s) r =>
  (NonEmpty (Item s) -> Sem r a) ->
  Sem r (Maybe (MenuAction a))
withSelection f =
  Just . maybe MenuAction.Continue MenuAction.success <$> withSelection' (fmap Just . f)

-- | Run an action with each entry in the selection or the focused entry and quit the menu with '()'.
-- If the menu was empty, do nothing (i.e. skip the event).
traverseSelection_ ::
  MenuState s =>
  Member (Menu s) r =>
  ((Item s) -> Sem r ()) ->
  Sem r (Maybe (MenuAction ()))
traverseSelection_ f =
  withSelection (traverse_ f)

adaptCursorAfterDeletion :: [Word] -> CursorIndex -> CursorIndex
adaptCursorAfterDeletion deleted (CursorIndex curs) =
  CursorIndex (foldl' f curs deleted)
  where
    f z i =
      if i <= curs && i > 0 then z - 1 else z

-- | Remove all entries for which @f@ returns @Just (Right b)@, or if none are matched, the last one for which @f@
-- returns @Just (Left a)@.
-- Return either:
-- * If some entries produced 'Right', the list of values contained in those 'Right's and @ents@ with those items
--   removed, in a 'Right'.
-- * If only a 'Left' was produced, the value contained in that 'Left', the score that indexes the 'IntMap', and the
--   index in the 'Seq' in the 'IntMap''s value, in a 'Left'.
-- * If no entry produced a 'Just', an empty list and the original @ents@ in a 'Right'.
popEntriesFallback ::
  (Int -> Entry i -> Maybe (Either a b)) ->
  Entries i ->
  Either (a, (Int, Int)) ([b], Entries i)
popEntriesFallback f ents =
  case trans (0, Left Nothing) ents of
    ((_, Right res), newEntries) -> Right (res, newEntries)
    ((_, Left (Just (res, newEntries))), _) -> Left (res, newEntries)
    ((_, Left Nothing), _) -> Right ([], ents)
  where
    trans =
      IntMap.mapAccumRWithKey \ z score ->
        (\ (z1, _, r) -> (z1, r)) . foldl' (transSeq score) (z, 0, Seq.empty)
    transSeq score ((i, z), scoreIndex, r) e =
      ((i + 1, res), scoreIndex + 1, if consumed then r else r |> e)
      where
        (res, consumed) =
          case z of
            Left a ->
              case f i e of
                Nothing -> (Left a, False)
                Just (Right b) -> (Right [b], True)
                Just (Left b) -> (Left (Just (b, (score, scoreIndex))), False)
            Right bs ->
              case f i e of
                Just (Right b) -> (Right (b : bs), True)
                _ -> (Right bs, False)

popSelection ::
  CursorIndex ->
  Entries i ->
  (([Word], [Int]), Entries i)
popSelection curs initial =
  unify (popEntriesFallback check initial)
  where
    check i e =
      justIf e.selected (Right (fromIntegral i, fromIntegral e.index)) <|>
      justIf (curs == fromIntegral i) (Left (fromIntegral e.index))
    unify = \case
      Right (entryAndItemIndexes, ent) ->
        (unzip entryAndItemIndexes, ent)
      Left (itemIndex, (score, scoreIndex)) ->
        (([fromIntegral curs], [itemIndex]), IntMap.update (Just . Seq.deleteAt scoreIndex) score initial)

deleteSelected ::
  MenuState s =>
  Member (State (WithCursor s)) r =>
  Sem r ()
deleteSelected = do
  curs <- use #cursor
  ((deletedEntries, deletedItems), kept) <- use (entries . to (popSelection curs))
  entries .= kept
  m <- use mode
  history m .= mempty
  items %= flip IntMap.withoutKeys (IntSet.fromList deletedItems)
  #cursor %= adaptCursorAfterDeletion deletedEntries

currentEntries ::
  MenuState s =>
  Member (Menu s) r =>
  Sem r [Text]
currentEntries =
  toListOf (sortedEntries . each . #item . #text) <$> readState

updateEntry ::
  MenuState s =>
  Member (State s) r =>
  Int ->
  Entry (Item s) ->
  Sem r ()
updateEntry index newEntry = do
  entries %= overEntry index (const newEntry)
  items . ix (fromIntegral newEntry.index) .= newEntry.item

modifyFocus ::
  MenuState s =>
  Member (Menu s) r =>
  (Entry (Item s) -> Entry (Item s)) ->
  Sem r ()
modifyFocus f =
  menuState do
    CursorIndex curs <- use #cursor
    use focusEntry >>= traverse_ \ e -> do
      updateEntry (fromIntegral curs) (f e)
      histories .= mempty
