module Ribosome.Menu.UpdateState where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as Text
import qualified Data.Trie as Trie
import Exon (exon)
import qualified Log

import Ribosome.Menu.Combinators (push)
import Ribosome.Menu.Data.Entry (Entries)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuEvent (MenuEvent, QueryEvent (History, Refined, Reset))
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuItems (MenuItems, MenuQuery (MenuQuery))
import qualified Ribosome.Menu.Effect.MenuFilter as MenuFilter
import Ribosome.Menu.Effect.MenuFilter (MenuFilter)
import Ribosome.Menu.Effect.MenuState (MenuState, itemsState)
import Ribosome.Menu.Lens (use, (%=), (+=), (.=))
import qualified Ribosome.Menu.Prompt.Data.Prompt as Prompt
import Ribosome.Menu.Prompt.Data.Prompt (PromptChange, PromptText (PromptText), unPromptText)

refineFiltered ::
  Members [MenuFilter, State (MenuItems i)] r =>
  MenuQuery ->
  Entries i ->
  Sem r QueryEvent
refineFiltered query ents = do
  push query =<< MenuFilter.refine query ents
  pure Refined

resetFiltered ::
  Members [MenuFilter, State (MenuItems i)] r =>
  MenuQuery ->
  Sem r QueryEvent
resetFiltered query = do
  its <- use #items
  new <- MenuFilter.initial query its
  #entries .= new
  #currentQuery .= query
  pure Reset

popFiltered ::
  Members [MenuFilter, State (MenuItems i)] r =>
  MenuQuery ->
  Sem r QueryEvent
popFiltered query@(MenuQuery (encodeUtf8 -> queryBs)) =
  maybe (resetFiltered query) matching =<< use (#history . to (`Trie.match` queryBs))
  where
    matching = \case
      (_, f, "") -> do
        #entries .= f
        #currentQuery .= query
        pure History
      (_, f, _) ->
        refineFiltered query f

appendFilter ::
  Members [MenuFilter, State (MenuItems i)] r =>
  MenuQuery ->
  Sem r QueryEvent
appendFilter query =
  ifM (use (#entries . to null)) (resetFiltered query) (refineFiltered query =<< use #entries)

promptChange ::
  Members [MenuFilter, State (MenuItems i)] r =>
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
  Members [MenuFilter, State (MenuItems i)] r =>
  [MenuItem i] ->
  Sem r ()
insertItems new = do
  index <- use #itemCount
  #itemCount += length new
  let newI = IntMap.fromList (zip [index..] new)
  #items %= IntMap.union newI
  query <- use #currentQuery
  ents <- MenuFilter.initial query newI
  #entries %= IntMap.unionWith (<>) ents
  unless (null ents) do
    #history .= mempty

promptItemUpdate ::
  Members [MenuFilter, State (MenuItems i)] r =>
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
  Members [MenuState i, MenuFilter, Events res MenuEvent, Log] r =>
  PromptText ->
  Sem r ()
queryUpdate prompt = do
  Log.debug [exon|query update: #{unPromptText prompt}|]
  itemsState do
    change <- use (#currentQuery . to (diffPrompt prompt))
    publish =<< promptItemUpdate change prompt
