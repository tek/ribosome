module Ribosome.Menu.UpdateState where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as Text
import qualified Data.Trie as Trie
import Exon (exon)
import qualified Log

import qualified Ribosome.Menu.Class.MenuState as MenuState
import Ribosome.Menu.Class.MenuState (
  Filter,
  MenuState,
  MenuState (Item, history, mode),
  entries,
  entryCount,
  filterMode,
  itemCount,
  items,
  )
import Ribosome.Menu.Combinators (push, updateEntries)
import Ribosome.Menu.Data.Entry (Entries, entriesLength)
import Ribosome.Menu.Data.State (MenuQuery (MenuQuery))
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuEvent (MenuEvent, QueryEvent (Modal, Refined, Reset))
import Ribosome.Menu.Data.MenuItem (MenuItem)
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
  fm <- use filterMode
  push query =<< menuFilter fm query (Refine ents)
  pure Refined

resetFiltered ::
  MenuState s =>
  Members [MenuFilter (Filter s), State s] r =>
  MenuQuery ->
  Sem r QueryEvent
resetFiltered query = do
  fm <- use filterMode
  its <- use items
  new <- menuFilter fm query (Initial its)
  updateEntries query new
  pure Reset

popFiltered ::
  MenuState s =>
  Members [MenuFilter (Filter s), State s] r =>
  MenuQuery ->
  Sem r QueryEvent
popFiltered query@(MenuQuery (encodeUtf8 -> queryBs)) = do
  m <- use mode
  maybe (resetFiltered query) matching =<< use (history m . to (`Trie.match` queryBs))
  where
    matching = \case
      (_, ents, "") -> do
        updateEntries query ents
        pure Modal
      (_, ents, _) ->
        refineFiltered query ents

-- TODO why does this not use popFiltered when entries are empty?
-- Also why does it not skip the action instead? Is there a situation in which there might have been added new items
-- without the insertion function updating entries?
appendFilter ::
  MenuState s =>
  Members [MenuFilter (Filter s), State s] r =>
  MenuQuery ->
  Sem r QueryEvent
appendFilter query =
  ifM (use (entries . to null)) (resetFiltered query) (refineFiltered query =<< use entries)

-- TODO all changes should check the history. deleting a character and adding it again does not change the result, but
-- refine will be called unconditionally. furthermore, unappend and random are identical.
promptChange ::
  MenuState s =>
  Members [MenuFilter (Filter s), State s] r =>
  PromptChange ->
  MenuQuery ->
  Sem r QueryEvent
promptChange = \case
  Prompt.Append ->
    appendFilter
  Prompt.Unappend ->
    popFiltered
  Prompt.Random ->
    popFiltered

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
  m <- use mode
  fm <- use filterMode
  query <- use MenuState.query
  ents <- menuFilter fm query (Initial newI)
  entries %= IntMap.unionWith (<>) ents
  entryCount += entriesLength ents
  unless (null ents) do
    history m .= mempty

promptItemUpdate ::
  MenuState s =>
  Members [MenuFilter (Filter s), State s] r =>
  PromptChange ->
  PromptText ->
  Sem r MenuEvent
promptItemUpdate change (PromptText (MenuQuery -> query)) =
  MenuEvent.Query <$> promptChange change query

diffPrompt :: PromptText -> MenuQuery -> PromptChange
diffPrompt (PromptText new) (MenuQuery old)
  | Text.isPrefixOf old new = Prompt.Append
  | Text.isPrefixOf new old = Prompt.Unappend
  | otherwise = Prompt.Random

queryEvent ::
  MenuState s =>
  Members [MenuFilter (Filter s), State s, Events res MenuEvent, Log] r =>
  Maybe PromptText ->
  Sem r ()
queryEvent = \case
  Just prompt -> do
    Log.debug [exon|query update: #{unPromptText prompt}|]
    change <- use (MenuState.query . to (diffPrompt prompt))
    publish =<< promptItemUpdate change prompt
  Nothing -> do
    query <- use MenuState.query
    publish . MenuEvent.Query =<< popFiltered query
