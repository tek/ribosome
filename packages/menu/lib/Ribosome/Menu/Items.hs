-- | Combinators for running actions on the selected or focused menu items.
-- Intended to be used from mapping handlers.
-- 'withFocus' and 'withSelection' will skip processing of the event downstream if the menu is empty.
module Ribosome.Menu.Items where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import Lens.Micro.Mtl (use, (%=), (.=))
import Prelude hiding (unify)

import Ribosome.Menu.Data.CursorIndex (CursorIndex (CursorIndex))
import qualified Ribosome.Menu.Data.Entry as Entry
import Ribosome.Menu.Data.Entry (Entries, Entry)
import Ribosome.Menu.Data.Menu (Menu)
import qualified Ribosome.Menu.Data.MenuAction as MenuAction
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuState (MenuWidget, semState)
import Ribosome.Menu.Effect.MenuState (MenuState, viewMenu)
import Ribosome.Menu.ItemLens (entries, focus, history, items, selected, selected')

-- |Run an action with the focused entry if the menu is non-empty.
withFocusItem ::
  Member (MenuState i) r =>
  (MenuItem i -> Sem r a) ->
  Sem r (Maybe a)
withFocusItem f =
  traverse f =<< viewMenu focus

-- |Run an action with the focused entry if the menu is non-empty, extracting the item payload.
withFocus' ::
  Member (MenuState i) r =>
  (i -> Sem r a) ->
  Sem r (Maybe a)
withFocus' f =
  withFocusItem (f . MenuItem.meta)

-- |Run an action with the focused entry and quit the menu with the returned value.
-- If the menu was empty, do nothing (i.e. skip the event).
withFocus ::
  Member (MenuState i) r =>
  (i -> Sem r a) ->
  MenuWidget r a
withFocus f =
  Just . maybe MenuAction.Continue MenuAction.success <$> withFocus' f

-- |Run an action with the selection or the focused entry if the menu is non-empty.
withSelectionItems ::
  Member (MenuState i) r =>
  (NonEmpty (MenuItem i) -> Sem r a) ->
  Sem r (Maybe a)
withSelectionItems f =
  traverse f =<< viewMenu selected'

-- |Run an action with the selection or the focused entry if the menu is non-empty, extracting the item payloads.
withSelection' ::
  Member (MenuState i) r =>
  (NonEmpty i -> Sem r a) ->
  Sem r (Maybe a)
withSelection' f =
  traverse f =<< viewMenu selected

-- |Run an action with the selection or the focused entry and quit the menu with the returned value.
-- If the menu was empty, do nothing (i.e. skip the event).
withSelection ::
  Member (MenuState i) r =>
  (NonEmpty i -> Sem r a) ->
  MenuWidget r a
withSelection f =
  Just . maybe MenuAction.Continue MenuAction.success <$> withSelection' f

-- |Run an action with each entry in the selection or the focused entry and quit the menu with '()'.
-- If the menu was empty, do nothing (i.e. skip the event).
traverseSelection_ ::
  Member (MenuState i) r =>
  (i -> Sem r ()) ->
  MenuWidget r ()
traverseSelection_ f =
  withSelection (traverse_ f)

adaptCursorAfterDeletion :: [Int] -> CursorIndex -> CursorIndex
adaptCursorAfterDeletion deleted (CursorIndex curs) =
  CursorIndex (foldl' f curs deleted)
  where
    f z i =
      if i <= curs && i > 0 then z - 1 else z

-- |Remove all entries for which @f@ returns @Just (Right b)@, or if none are matched, the last one for which @f@
-- returns @Just (Left a)@.
-- Return either:
-- * If some entries produced 'Right', the list of values contained in those 'Right's and @ents@ with those items
--   removed, in a 'Right'.
-- * If only a 'Left' was produced, the value contained in that 'Left', the score that indexes the 'IntMap', and the
--   index in the 'Seq' in the 'IntMap''s value.
-- * If no entry produced a 'Just', an empty list and the original @ents@ in a 'Left'.
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
    transSeq score ((i, z), si, r) e =
      ((i + 1, res), si + 1, if consumed then r else r |> e)
      where
        (res, consumed) =
          case z of
            Left a ->
              case f i e of
                Nothing -> (Left a, False)
                Just (Right b) -> (Right [b], True)
                Just (Left b) -> (Left (Just (b, (score, si))), False)
            Right bs ->
              case f i e of
                Just (Right b) -> (Right (b : bs), True)
                _ -> (Right bs, False)

justIf :: Bool -> a -> Maybe a
justIf cond a =
  bool Nothing (Just a) cond

popSelection ::
  Int ->
  Entries i ->
  (([Int], [Int]), Entries i)
popSelection curs initial =
  unify (popEntriesFallback check initial)
  where
    check i e =
      justIf (Entry.selected e) (Right (i, Entry.index e)) <|>
      justIf (curs == i) (Left (Entry.index e))
    unify = \case
      Right (is, ent) ->
        (unzip is, ent)
      Left (i, (score, si)) ->
        (([curs], [i]), IntMap.update (Just . Seq.deleteAt si) score initial)

deleteSelected ::
  Member (State (Menu i)) r =>
  Sem r ()
deleteSelected =
  semState do
    CursorIndex curs <- use #cursor
    ((deletedEntries, deletedItems), kept) <- use (entries . to (popSelection curs))
    entries .= kept
    history .= mempty
    items %= flip IntMap.withoutKeys (IntSet.fromList deletedItems)
    #cursor %= adaptCursorAfterDeletion deletedEntries
