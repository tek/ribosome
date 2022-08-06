module Ribosome.Menu.Filters where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Lens.Micro.Mtl (view)
import Text.FuzzyFind (Alignment (Alignment), bestMatch)

import qualified Ribosome.Menu.Data.Entry as Entry
import Ribosome.Menu.Data.Entry (Entries, Entry (Entry))
import Ribosome.Menu.Data.MenuItems (MenuQuery (MenuQuery))
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Data.MenuItem (Items, MenuItem (MenuItem))
import qualified Ribosome.Menu.Stream.ParMap as Stream

textContains :: Text -> Text -> Bool
textContains needle haystack =
  Text.null needle || (not (Text.null haystack) && search needle haystack)
  where
    search n h =
      not (Text.null (snd (Text.breakOn n h)))

entry :: Int -> MenuItem a -> Entry a
entry index item =
  Entry item index False

matchSubstring :: Text -> Int -> MenuItem a -> Maybe (Int, Entry a)
matchSubstring query index item =
  bool Nothing (Just (0, entry index item)) (textContains query (MenuItem.text item))

initialSubstring :: MenuQuery -> Items a -> (Entries a)
initialSubstring (MenuQuery query) items =
  IntMap.singleton 0 (Seq.fromList (uncurry entry <$> trans (IntMap.toList items)))
  where
    trans =
      if Text.null query then id else filter matcher
    matcher =
      textContains query . MenuItem.text . snd

refineSubstring :: MenuQuery -> Entries a -> Entries a
refineSubstring (MenuQuery query) items =
  IntMap.singleton 0 (Seq.fromList (trans (concatMap toList items)))
  where
    trans =
      if Text.null query then id else filter matcher
    matcher =
      textContains query . view (#item . #text)

matchFuzzy :: Bool -> Text -> Int -> MenuItem a -> Maybe (Int, Entry a)
matchFuzzy sel (toString -> query) index item@(MenuItem _ (toString -> text) _) = do
  Alignment score _ <- bestMatch query text
  pure (score, Entry item index sel)

-- |This variant matches fuzzily but reserves the order of items.
filterFuzzyMonotonic ::
  String ->
  [(Int, MenuItem a)] ->
  IO [Entry a]
filterFuzzyMonotonic = \case
  "" ->
    pure . fmap (uncurry entry)
  (toText -> query) ->
    fmap (fmap snd) <$> Stream.parMapMaybeIO 100 (uncurry (matchFuzzy False query))

filterFuzzy ::
  Bool ->
  String ->
  [(Int, MenuItem a)] ->
  IO (Entries a)
filterFuzzy True q =
  fmap (IntMap.singleton 0 . Seq.fromList) . filterFuzzyMonotonic q
-- if the query is empty, score shorter strings higher
filterFuzzy False "" =
  pure .
  Entry.fromList .
  fmap \ (i, item) -> (1000 - Text.length (MenuItem.text item), Entry item i False)
filterFuzzy False (toText -> query) =
  fmap Entry.fromList .
  Stream.parMapMaybeIO 100 (uncurry (matchFuzzy False query))

refineFuzzy ::
  String ->
  [Entry a] ->
  IO (Entries a)
refineFuzzy (toText -> query) =
  fmap Entry.fromList .
  Stream.parMapMaybeIO 100 match
  where
    match (Entry item index sel) =
      matchFuzzy sel query index item
