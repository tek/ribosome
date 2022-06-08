module Ribosome.Menu.Filters where

import Control.Lens (view)
import Data.Composition ((.:))
import Data.Generics.Labels ()
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Text.FuzzyFind (Alignment (Alignment), bestMatch)

import qualified Ribosome.Menu.Data.Entry as Entry
import Ribosome.Menu.Data.Entry (Entries, Entry (Entry))
import Ribosome.Menu.Data.MenuData (MenuQuery (MenuQuery))
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Data.MenuItem (MenuItem (MenuItem))
import Ribosome.Menu.Data.MenuItemFilter (MenuItemFilter (MenuItemFilter))
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
          textContains query . view (#item . #text)
    initial (MenuQuery query) items =
      IntMap.singleton 0 (Seq.fromList (uncurry entry <$> trans (IntMap.toList items)))
      where
        trans =
          if Text.null query then id else filter matcher
        matcher =
          textContains query . MenuItem.text . snd

matchFuzzy :: Bool -> Text -> Int -> MenuItem a -> Maybe (Int, Entry a)
matchFuzzy sel (toString -> query) index item@(MenuItem _ (toString -> text) _) = do
  Alignment score _ <- bestMatch query text
  pure (score, Entry item index sel)

-- if the query is empty, score shorter strings higher
filterFuzzy ::
  Bool ->
  String ->
  [(Int, MenuItem a)] ->
  IO (Entries a)
filterFuzzy False "" =
  pure . IntMap.singleton 0 . Seq.fromList . fmap (uncurry entry)
filterFuzzy True "" =
  pure .
  Entry.fromList .
  fmap \ (i, item) -> (1000 - Text.length (MenuItem.text item), Entry item i False)
filterFuzzy _ (toText -> query) =
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

fuzzyItemFilter :: Bool -> MenuItemFilter a
fuzzyItemFilter sortEmpty =
  MenuItemFilter (matchFuzzy False) initial refine
  where
    initial (MenuQuery (toString -> query)) items =
      filterFuzzy sortEmpty query (IntMap.toList items)
    refine (MenuQuery (toString -> query)) items =
      refineFuzzy query (concatMap toList (IntMap.elems items))

fuzzy :: MenuItemFilter a
fuzzy =
  fuzzyItemFilter True
