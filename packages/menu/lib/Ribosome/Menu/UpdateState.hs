module Ribosome.Menu.UpdateState where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as Text
import qualified Data.Trie as Trie
import Exon (exon)
import qualified Log

import Ribosome.Menu.Combinators (addHistory, push)
import Ribosome.Menu.Data.Entry (Entries)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuEvent (MenuEvent, QueryEvent (History, Refined, Reset))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuItems (MenuItems, MenuQuery (MenuQuery))
import qualified Ribosome.Menu.Effect.MenuFilter as MenuFilter
import Ribosome.Menu.Effect.MenuFilter (MenuFilter)
import Ribosome.Menu.Lens (use, (%=), (+=), (.=))
import qualified Ribosome.Menu.Prompt.Data.Prompt as Prompt
import Ribosome.Menu.Prompt.Data.Prompt (PromptChange, PromptText (PromptText), unPromptText)

refineFiltered ::
  Ord filter =>
  Members [MenuFilter filter, State (MenuItems filter i)] r =>
  MenuQuery ->
  Entries i ->
  Sem r QueryEvent
refineFiltered query ents = do
  filt <- use #currentFilter
  push query =<< MenuFilter.refine filt query ents
  pure Refined

resetFiltered ::
  Members [MenuFilter filter, State (MenuItems filter i)] r =>
  MenuQuery ->
  Sem r QueryEvent
resetFiltered query = do
  filt <- use #currentFilter
  its <- use #items
  new <- MenuFilter.initial filt query its
  #entries .= new
  #currentQuery .= query
  pure Reset

popFiltered ::
  Ord filter =>
  Members [MenuFilter filter, State (MenuItems filter i)] r =>
  MenuQuery ->
  Sem r QueryEvent
popFiltered query@(MenuQuery (encodeUtf8 -> queryBs)) = do
  filt <- use #currentFilter
  maybe (resetFiltered query) matching =<< use (#history . ix filt . to (`Trie.match` queryBs))
  where
    matching = \case
      (_, f, "") -> do
        #entries .= f
        #currentQuery .= query
        pure History
      (_, f, _) ->
        refineFiltered query f

-- TODO why does this not use popFiltered when entries are empty?
-- Also why does it not skip the action instead? Is there a situation in which there might have been added new items
-- without the insertion function updating entries?
appendFilter ::
  Ord filter =>
  Members [MenuFilter filter, State (MenuItems filter i)] r =>
  MenuQuery ->
  Sem r QueryEvent
appendFilter query =
  ifM (use (#entries . to null)) (resetFiltered query) (refineFiltered query =<< use #entries)

promptChange ::
  Ord filter =>
  Members [MenuFilter filter, State (MenuItems filter i)] r =>
  PromptChange ->
  MenuQuery ->
  Sem r QueryEvent
promptChange = \case
  Prompt.Append ->
    appendFilter
  Prompt.Unappend ->
    popFiltered
  Prompt.Random ->
    resetFiltered

insertItems ::
  Ord filter =>
  Members [MenuFilter filter, State (MenuItems filter i)] r =>
  [MenuItem i] ->
  Sem r ()
insertItems new = do
  index <- use #itemCount
  #itemCount += length new
  let newI = IntMap.fromList (zip [index..] new)
  #items %= IntMap.union newI
  filt <- use #currentFilter
  query <- use #currentQuery
  ents <- MenuFilter.initial filt query newI
  #entries %= IntMap.unionWith (<>) ents
  unless (null ents) do
    #history . ix filt .= mempty

promptItemUpdate ::
  Ord filter =>
  Members [MenuFilter filter, State (MenuItems filter i)] r =>
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

queryUpdate ::
  Ord filter =>
  Members [MenuFilter filter, State (MenuItems filter i), Events res MenuEvent, Log] r =>
  PromptText ->
  Sem r ()
queryUpdate prompt = do
  Log.debug [exon|query update: #{unPromptText prompt}|]
  change <- use (#currentQuery . to (diffPrompt prompt))
  publish =<< promptItemUpdate change prompt

changeFilter ::
  Ord filter =>
  Show filter =>
  Members [MenuFilter filter, State (MenuItems filter i), Events res MenuEvent, Log] r =>
  filter ->
  Sem r ()
changeFilter new = do
  old <- use #currentFilter
  when (new /= old) do
    Log.debug [exon|change filter: #{show new}|]
    addHistory
    #currentFilter .= new
    query <- use #currentQuery
    publish . MenuEvent.Query =<< popFiltered query
