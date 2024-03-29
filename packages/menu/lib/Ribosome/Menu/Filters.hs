module Ribosome.Menu.Filters where

import Control.Lens.Regex.Text (Regex, match, regexing)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as Text
import Text.FuzzyFind (Alignment (Alignment), bestMatch)

import Ribosome.Menu.Class.MenuMode (Extractor, Matcher)
import qualified Ribosome.Menu.Data.Entry as Entry
import Ribosome.Menu.Data.Entry (Entries, Entry (Entry))
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Data.MenuItem (Items, MenuItem)
import qualified Ribosome.Menu.Stream.ParMap as Stream

entry :: Int -> MenuItem i -> Entry i
entry index item =
  Entry item (fromIntegral index) False

filterPar ::
  ∀ a i .
  Extractor i a ->
  Matcher i a ->
  [Entry i] ->
  IO (Entries i)
filterPar ex f =
  fmap Entry.fromList .
  Stream.parMapMaybeIO 100 (f ex)

initPar ::
  ∀ a i .
  Extractor i a ->
  Matcher i a ->
  Items i ->
  IO (Entries i)
initPar ex f =
  filterPar ex f .
  fmap (uncurry entry) .
  IntMap.toList

refinePar ::
  ∀ a i .
  Extractor i a ->
  Matcher i a ->
  Entries i ->
  IO (Entries i)
refinePar ex f =
  filterPar ex f .
  concatMap toList

matchOrEmpty ::
  (Text -> Bool) ->
  Maybe Text ->
  Bool
matchOrEmpty predicate = \case
  Just text ->
    not (Text.null text) && predicate text
  Nothing ->
    False

matchAll :: Matcher i a
matchAll _ e =
  Just (0, e)

matchSimple ::
  (Text -> Bool) ->
  Matcher i Text
matchSimple predicate ex e =
  bool Nothing (Just (0, e)) (matchOrEmpty predicate (ex (e ^. #item)))

matchSubstring :: Text -> Matcher i Text
matchSubstring query =
  matchSimple (Text.isInfixOf query)

matchPrefix :: Text -> Matcher i Text
matchPrefix query =
  matchSimple (Text.isPrefixOf query)

matchRegex :: Regex -> Matcher i Text
matchRegex query =
  matchSimple (has (rx . match))
  where
    rx =
      regexing query

-- |If the first arg is 'True', matches fuzzily but preserves the order of items for the empty query.
matchFuzzy :: Bool -> String -> Matcher i Text
-- |If the query is empty, score shorter strings higher.
matchFuzzy False "" _ e =
  Just (-(Text.length e.item.text), e)
matchFuzzy True "" _ e =
  Just (0, e)
matchFuzzy _ query ex (Entry item@(ex -> Just seg) index sel) = do
  Alignment score _ <- bestMatch query (toString seg)
  pure (score, Entry item index sel)
matchFuzzy _ _ _ _ =
  Nothing
