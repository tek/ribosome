module Ribosome.Menu.Filters where

import Control.Lens.Regex.Text (Regex, match, regexing)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as Text
import Text.FuzzyFind (Alignment (Alignment), bestMatch)

import qualified Ribosome.Menu.Data.Entry as Entry
import Ribosome.Menu.Data.Entry (Entries, Entry (Entry))
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Data.MenuItem (Items, MenuItem)
import qualified Ribosome.Menu.Stream.ParMap as Stream

type Matcher i =
  (MenuItem i -> Text) -> Entry i -> Maybe (Int, Entry i)

entry :: Int -> MenuItem i -> Entry i
entry index item =
  Entry item index False

filterPar ::
  (MenuItem i -> Text) ->
  Matcher i ->
  [Entry i] ->
  IO (Entries i)
filterPar ex f =
  fmap Entry.fromList .
  Stream.parMapMaybeIO 100 (f ex)

initPar ::
  (MenuItem i -> Text) ->
  Matcher i ->
  Items i ->
  IO (Entries i)
initPar ex f =
  filterPar ex f .
  fmap (uncurry entry) .
  IntMap.toList

refinePar ::
  (MenuItem i -> Text) ->
  Matcher i ->
  Entries i ->
  IO (Entries i)
refinePar ex f =
  filterPar ex f .
  concatMap toList

matchOrEmpty ::
  (Text -> Bool) ->
  Text ->
  Bool
matchOrEmpty predicate text =
  not (Text.null text) && predicate text

matchAll :: Matcher i
matchAll _ e =
  Just (0, e)

matchSimple ::
  (Text -> Bool) ->
  Matcher i
matchSimple predicate ex e =
  bool Nothing (Just (0, e)) (matchOrEmpty predicate (ex (e ^. #item)))

matchSubstring :: Text -> Matcher i
matchSubstring query =
  matchSimple (Text.isInfixOf query)

matchPrefix :: Text -> Matcher i
matchPrefix query =
  matchSimple (Text.isPrefixOf query)

matchRegex :: Regex -> Matcher i
matchRegex query =
  matchSimple (has (rx . match))
  where
    rx =
      regexing query

-- |If the first arg is 'True', matches fuzzily but preserves the order of items for the empty query.
matchFuzzy :: Bool -> String -> Matcher i
-- |If the query is empty, score shorter strings higher.
matchFuzzy False "" _ e =
  Just (-(Text.length (MenuItem.text (e ^. #item))), e)
matchFuzzy True "" _ e =
  Just (0, e)
matchFuzzy _ query ex (Entry item index sel) = do
  Alignment score _ <- bestMatch query (toString (ex item))
  pure (score, Entry item index sel)
