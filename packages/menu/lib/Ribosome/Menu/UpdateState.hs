module Ribosome.Menu.UpdateState where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as Text
import qualified Data.Trie as Trie
import Exon (exon)
import qualified Log

import qualified Ribosome.Menu.Class.MenuState as MenuState
import Ribosome.Menu.Class.MenuState (
  Filter,
  MenuState (Item, history),
  entries,
  entryCount,
  itemCount,
  items,
  )
import Ribosome.Menu.Combinators (push, updateEntries)
import Ribosome.Menu.Data.Entry (Entries, entriesLength)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuEvent (MenuEvent, QueryEvent (Modal, Refined, Reset))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.State (MenuQuery (MenuQuery))
import Ribosome.Menu.Effect.MenuFilter (FilterJob (Initial, Refine), MenuFilter, menuFilter)
import Ribosome.Menu.Lens (use, (%=), (+=), (.=))
import qualified Ribosome.Menu.Prompt.Data.Prompt as Prompt
import Ribosome.Menu.Prompt.Data.Prompt (PromptChange, PromptText (PromptText), unPromptText)

refineFiltered ::
  MenuState s =>
  Members [MenuFilter (Filter s), State s] r =>
  MenuQuery ->
  Entries (Item s) ->
  Sem r QueryEvent
refineFiltered query ents = do
  filterMode <- use MenuState.filterMode
  push query =<< menuFilter filterMode query (Refine ents)
  pure Refined

resetFiltered ::
  MenuState s =>
  Members [MenuFilter (Filter s), State s] r =>
  MenuQuery ->
  Sem r (Maybe QueryEvent)
resetFiltered query = do
  filterMode <- use MenuState.filterMode
  its <- use items
  new <- menuFilter filterMode query (Initial its)
  updateEntries query new
  pure (Just Reset)

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

popFiltered ::
  MenuState s =>
  Members [MenuFilter (Filter s), State s] r =>
  MenuQuery ->
  Sem r (Maybe QueryEvent)
popFiltered =
  historyOr resetFiltered

appendFilter ::
  MenuState s =>
  Members [MenuFilter (Filter s), State s] r =>
  MenuQuery ->
  Sem r (Maybe QueryEvent)
appendFilter query = do
  es <- use entries
  if null es
  then pure Nothing
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
  mode <- use MenuState.mode
  filterMode <- use MenuState.filterMode
  query <- use MenuState.query
  ents <- menuFilter filterMode query (Initial newI)
  entries %= IntMap.unionWith (<>) ents
  entryCount += entriesLength ents
  history mode .= mempty

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
    popFiltered =<< use MenuState.query

queryEvent ::
  MenuState s =>
  Members [MenuFilter (Filter s), State s, Events res MenuEvent, Log] r =>
  Maybe PromptText ->
  Sem r ()
queryEvent =
  traverse_ (publish . MenuEvent.Query) <=< updateQuery
