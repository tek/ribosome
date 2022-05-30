module Ribosome.Menu.Filters where

import Control.Lens (view, (^.))
import Data.Composition ((.:))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Prelude as Stream
import Text.FuzzyFind (Alignment (Alignment), bestMatch)

import qualified Ribosome.Menu.Data.Entry as Entry
import Ribosome.Menu.Data.Entry (Entries, Entry (Entry))
import Ribosome.Menu.Data.MenuData (MenuQuery (MenuQuery))
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Data.MenuItem (MenuItem (MenuItem))
import Ribosome.Menu.Data.MenuItemFilter (MenuItemFilter (MenuItemFilter))

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
  bool Nothing (Just (0, entry index item)) (textContains query (item ^. MenuItem.text))

substringItemFilter :: MenuItemFilter a
substringItemFilter =
  MenuItemFilter matchSubstring (pure .: initial) (pure .: refine)
  where
    refine (MenuQuery query) items =
      IntMap.singleton 0 (Seq.fromList (trans (concatMap toList items)))
      where
        trans =
          if Text.null query then id else filter matcher
        matcher =
          textContains query . view (Entry.item . MenuItem.text)
    initial (MenuQuery query) items =
      IntMap.singleton 0 (Seq.fromList (uncurry entry <$> trans (IntMap.toList items)))
      where
        trans =
          if Text.null query then id else filter matcher
        matcher =
          textContains query . view MenuItem.text . snd

matchFuzzy :: Bool -> Text -> Int -> MenuItem a -> Maybe (Int, Entry a)
matchFuzzy sel (toString -> query) index item@(MenuItem _ (toString -> text) _) = do
  Alignment score _ <- bestMatch query text
  pure (score, Entry item index sel)

-- if the query is empty, score shorter strings higher
filterFuzzyPar ::
  Bool ->
  String ->
  [(Int, MenuItem a)] ->
  IO (Entries a)
filterFuzzyPar False "" =
  pure . IntMap.singleton 0 . Seq.fromList . fmap (uncurry entry)
filterFuzzyPar True "" =
  pure .
  Entry.fromList .
  fmap \ (i, item) -> (1000 - Text.length (item ^. MenuItem.text), Entry item i False)
filterFuzzyPar _ (toText -> query) =
  fmap Entry.fromList .
  Stream.toList .
  Stream.maxThreads 12 .
  Stream.fromParallel .
  Stream.concatMapWith Stream.parallel (Stream.fromList . mapMaybe (uncurry (matchFuzzy False query))) .
  Stream.chunksOf 100 Fold.toList .
  Stream.fromList

refineFuzzyPar ::
  String ->
  [Entry a] ->
  IO (Entries a)
refineFuzzyPar (toText -> query) =
  fmap Entry.fromList .
  Stream.toList .
  Stream.maxThreads 12 .
  Stream.fromParallel .
  Stream.concatMapWith Stream.parallel (Stream.fromList . mapMaybe match) .
  Stream.chunksOf 100 Fold.toList .
  Stream.fromList
  where
    match (Entry item index sel) =
      matchFuzzy sel query index item

fuzzyItemFilterPar :: Bool -> MenuItemFilter a
fuzzyItemFilterPar sortEmpty =
  MenuItemFilter (matchFuzzy False) initial refine
  where
    initial (MenuQuery (toString -> query)) items =
      filterFuzzyPar sortEmpty query (IntMap.toList items)
    refine (MenuQuery (toString -> query)) items =
      refineFuzzyPar query (concatMap toList (IntMap.elems items))

-- if the query is empty, score shorter strings higher
filterFuzzy ::
  String ->
  [(Int, MenuItem a)] ->
  Entries a
filterFuzzy "" =
  Entry.fromList . fmap \ (i, item) ->
    (1000 - Text.length (item ^. MenuItem.text), Entry item i False)
filterFuzzy query =
  Entry.fromList .
  mapMaybe (uncurry match)
  where
    match index item@(MenuItem _ (toString -> text) _) = do
      Alignment score _ <- bestMatch query text
      pure (score, Entry item index False)

refineFuzzy ::
  String ->
  [Entry a] ->
  Entries a
refineFuzzy query =
  Entry.fromList .
  mapMaybe match
  where
    match (Entry item@(MenuItem _ (toString -> text) _) index sel) = do
      Alignment score _ <- bestMatch query text
      pure (score, Entry item index sel)

fuzzyItemFilter :: MenuItemFilter a
fuzzyItemFilter =
  MenuItemFilter (matchFuzzy False) (pure .: initial) (pure .: refine)
  where
    initial (MenuQuery (toString -> query)) items =
      filterFuzzy query (IntMap.toList items)
    refine (MenuQuery (toString -> query)) items =
      refineFuzzy query (concatMap toList (IntMap.elems items))
