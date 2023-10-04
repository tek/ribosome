module Ribosome.Menu.UpdateState where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as Text
import qualified Data.Trie as Trie
import Exon (exon)
import qualified Log

import qualified Ribosome.Menu.Class.MenuState as MenuState
import Ribosome.Menu.Class.MenuState (MenuState (Item, histories), entries, entryCount, history, itemCount, items)
import Ribosome.Menu.Combinators (addHistory, updateEntries)
import Ribosome.Menu.Data.Entry (Entries, entriesLength)
import Ribosome.Menu.Data.MenuEvent (QueryEvent (Modal, Refined, Reset))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuQuery (MenuQuery (MenuQuery))
import Ribosome.Menu.Effect.MenuFilter (FilterJob (Initial, Refine), MenuFilter, menuFilter)
import Ribosome.Menu.Lens (use, (%=), (+=), (.=))
import qualified Ribosome.Menu.Prompt.Data.Prompt as Prompt
import Ribosome.Menu.Prompt.Data.Prompt (PromptChange, PromptText (PromptText))

refilter ::
  MenuState s =>
  Members [MenuFilter, State s] r =>
  MenuQuery ->
  FilterJob (Item s) (Entries (Item s)) ->
  Sem r ()
refilter query job = do
  filterMode <- use MenuState.mode
  updateEntries query =<< menuFilter filterMode query job

refineFiltered ::
  MenuState s =>
  Members [MenuFilter, State s] r =>
  MenuQuery ->
  Entries (Item s) ->
  Sem r QueryEvent
refineFiltered query ents =
  Refined <$ refilter query (Refine ents)

resetFiltered ::
  MenuState s =>
  Members [MenuFilter, State s] r =>
  MenuQuery ->
  Sem r (Maybe QueryEvent)
resetFiltered query = do
  its <- use items
  Just Reset <$ refilter query (Initial its)

historyOr ::
  MenuState s =>
  Members [MenuFilter, State s] r =>
  (MenuQuery -> Sem r (Maybe QueryEvent)) ->
  MenuQuery ->
  Sem r (Maybe QueryEvent)
historyOr noMatch query@(MenuQuery (encodeUtf8 -> queryBs)) = do
  mode <- use MenuState.mode
  hist <- use (history mode)
  maybe (noMatch query) matching (hist >>= flip Trie.match queryBs)
  where
    matching = \case
      (_, ents, "") -> do
        updateEntries query ents
        pure (Just Modal)
      (_, ents, _) ->
        Just <$> refineFiltered query ents

-- TODO this returns Just Refined for empty entries so that a race condition in tests can be avoided.
-- improve this
-- also it appears not to reliably solve the race condition
appendFilter ::
  MenuState s =>
  Members [MenuFilter, State s] r =>
  MenuQuery ->
  Sem r (Maybe QueryEvent)
appendFilter query = do
  es <- use entries
  if null es
  then pure (Just Refined)
  else Just <$> refineFiltered query es

promptChange ::
  MenuState s =>
  Members [MenuFilter, State s] r =>
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
  Members [MenuFilter, State s, Log] r =>
  [MenuItem (Item s)] ->
  Sem r ()
insertItems new = do
  index <- use itemCount
  itemCount += newCount
  let newI = IntMap.fromList (zip [fromIntegral index..] new)
  items %= IntMap.union newI
  filterMode <- use MenuState.mode
  query <- use MenuState.query
  ents <- menuFilter filterMode query (Initial newI)
  entries %= IntMap.unionWith (<>) ents
  let newEntriesCount = entriesLength ents
  entryCount += newEntriesCount
  histories .= mempty
  addHistory query =<< use entries
  Log.debug [exon|Inserted #{show newCount} items (#{show newEntriesCount} entries)|]
  where
    newCount = fromIntegral (length new)

promptItemUpdate ::
  MenuState s =>
  Members [MenuFilter, State s] r =>
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
  Members [MenuFilter, State s, Log] r =>
  Maybe PromptText ->
  Sem r (Maybe QueryEvent)
updateQuery = \case
  Just prompt -> do
    Log.debug [exon|query update: ##{prompt}|]
    change <- use (MenuState.query . to (diffPrompt prompt))
    promptItemUpdate change prompt
  Nothing -> do
    Log.debug "query reset"
    historyOr resetFiltered =<< use MenuState.query
