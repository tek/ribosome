module Ribosome.Menu.UpdateState where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as Text
import qualified Data.Trie as Trie
import Exon (exon)
import qualified Log

import qualified Ribosome.Menu.Class.MenuState as MenuState
import Ribosome.Menu.Class.MenuState (
  Filter,
  MenuState (Item, histories),
  entries,
  entryCount,
  history,
  itemCount,
  items,
  )
import Ribosome.Menu.Combinators (addHistory, updateEntries)
import Ribosome.Menu.Data.Entry (Entries, entriesLength)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuEvent (MenuEvent, QueryEvent (Modal, Refined, Reset))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.State (MenuQuery (MenuQuery))
import Ribosome.Menu.Effect.MenuFilter (FilterJob (Initial, Refine), MenuFilter, menuFilter)
import Ribosome.Menu.Lens (use, (%=), (+=), (.=))
import qualified Ribosome.Menu.Prompt.Data.Prompt as Prompt
import Ribosome.Menu.Prompt.Data.Prompt (PromptChange, PromptText (PromptText), unPromptText)

refilter ::
  MenuState s =>
  Members [MenuFilter (Filter s), State s] r =>
  MenuQuery ->
  FilterJob (Item s) (Entries (Item s)) ->
  Sem r ()
refilter query job = do
  filterMode <- use MenuState.filterMode
  updateEntries query =<< menuFilter filterMode query job

refineFiltered ::
  MenuState s =>
  Members [MenuFilter (Filter s), State s] r =>
  MenuQuery ->
  Entries (Item s) ->
  Sem r QueryEvent
refineFiltered query ents =
  Refined <$ refilter query (Refine ents)

resetFiltered ::
  MenuState s =>
  Members [MenuFilter (Filter s), State s] r =>
  MenuQuery ->
  Sem r (Maybe QueryEvent)
resetFiltered query = do
  its <- use items
  Just Reset <$ refilter query (Initial its)

historyOr ::
  MenuState s =>
  Members [MenuFilter (Filter s), State s] r =>
  (MenuQuery -> Sem r (Maybe QueryEvent)) ->
  MenuQuery ->
  Sem r (Maybe QueryEvent)
historyOr noMatch query@(MenuQuery (encodeUtf8 -> queryBs)) = do
  mode <- use MenuState.mode
  maybe (noMatch query) matching =<< use (history mode . to (`Trie.match` queryBs))
  where
    matching = \case
      (_, ents, "") -> do
        updateEntries query ents
        pure (Just Modal)
      (_, ents, _) ->
        Just <$> refineFiltered query ents

-- TODO this returns Just Refined for empty entries so that a race condition in tests can be avoided.
-- improve this
appendFilter ::
  MenuState s =>
  Members [MenuFilter (Filter s), State s] r =>
  MenuQuery ->
  Sem r (Maybe QueryEvent)
appendFilter query = do
  es <- use entries
  if null es
  then pure (Just Refined)
  else Just <$> refineFiltered query es

promptChange ::
  MenuState s =>
  Members [MenuFilter (Filter s), State s] r =>
  PromptChange ->
  MenuQuery ->
  Sem r (Maybe QueryEvent)
promptChange = \case
  Prompt.Append ->
    appendFilter
  Prompt.Random ->
    resetFiltered

insertItems ::
  MenuState s =>
  Members [MenuFilter (Filter s), State s] r =>
  [MenuItem (Item s)] ->
  Sem r ()
insertItems new = do
  index <- use itemCount
  itemCount += length new
  let newI = IntMap.fromList (zip [index..] new)
  items %= IntMap.union newI
  filterMode <- use MenuState.filterMode
  query <- use MenuState.query
  ents <- menuFilter filterMode query (Initial newI)
  entries %= IntMap.unionWith (<>) ents
  entryCount += entriesLength ents
  histories .= mempty
  addHistory query =<< use entries

promptItemUpdate ::
  MenuState s =>
  Members [MenuFilter (Filter s), State s] r =>
  PromptChange ->
  PromptText ->
  Sem r (Maybe QueryEvent)
promptItemUpdate change (PromptText (MenuQuery -> query)) =
  historyOr (promptChange change) query

diffPrompt :: PromptText -> MenuQuery -> PromptChange
diffPrompt (PromptText new) (MenuQuery old)
  | Text.isPrefixOf old new = Prompt.Append
  | otherwise = Prompt.Random

updateQuery ::
  MenuState s =>
  Members [MenuFilter (Filter s), State s, Log] r =>
  Maybe PromptText ->
  Sem r (Maybe QueryEvent)
updateQuery = \case
  Just prompt -> do
    Log.debug [exon|query update: #{unPromptText prompt}|]
    change <- use (MenuState.query . to (diffPrompt prompt))
    promptItemUpdate change prompt
  Nothing ->
    historyOr resetFiltered =<< use MenuState.query

queryEvent ::
  MenuState s =>
  Members [MenuFilter (Filter s), State s, Events res MenuEvent, Log] r =>
  Maybe PromptText ->
  Sem r ()
queryEvent =
  traverse_ (publish . MenuEvent.Query) <=< updateQuery
