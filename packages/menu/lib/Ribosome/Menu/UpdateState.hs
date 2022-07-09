module Ribosome.Menu.UpdateState where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as Text
import qualified Data.Trie as Trie
import Exon (exon)
import Lens.Micro.Mtl (use, (%=), (+=), (.=))
import qualified Polysemy.Log as Log

import Ribosome.Menu.Combinators (push)
import Ribosome.Menu.Data.Entry (Entries)
import Ribosome.Menu.Data.MenuData (MenuItems, MenuQuery (MenuQuery))
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuState (SemS (SemS), semState)
import qualified Ribosome.Menu.Data.QuitReason as QuitReason
import qualified Ribosome.Menu.Effect.MenuFilter as MenuFilter
import Ribosome.Menu.Effect.MenuFilter (MenuFilter)
import Ribosome.Menu.Effect.MenuState (MenuState, readPrompt, setPrompt, useItems)
import Ribosome.Menu.Prompt.Data.Prompt (
  Prompt (Prompt),
  PromptChange (PromptAppend, PromptRandom, PromptUnappend),
  PromptText (PromptText),
  )
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)

refineFiltered ::
  Member MenuFilter r =>
  MenuQuery ->
  Entries i ->
  SemS (MenuItems i) r ()
refineFiltered query ents =
  push query =<< SemS (MenuFilter.refine query ents)

resetFiltered ::
  Member MenuFilter r =>
  MenuQuery ->
  SemS (MenuItems i) r ()
resetFiltered query = do
  its <- use #items
  new <- SemS (MenuFilter.initial query its)
  #entries .= new
  #currentQuery .= query

popFiltered ::
  Member MenuFilter r =>
  MenuQuery ->
  SemS (MenuItems i) r ()
popFiltered query@(MenuQuery (encodeUtf8 -> queryBs)) =
  maybe (resetFiltered query) matching =<< use (#history . to (`Trie.match` queryBs))
  where
    matching = \case
      (_, f, "") -> do
        #entries .= f
        #currentQuery .= query
      (_, f, _) ->
        refineFiltered query f

appendFilter ::
  Member MenuFilter r =>
  MenuQuery ->
  SemS (MenuItems i) r ()
appendFilter query =
  ifM (use (#entries . to null)) (resetFiltered query) (refineFiltered query =<< use #entries)

promptChange ::
  Member MenuFilter r =>
  PromptChange ->
  MenuQuery ->
  SemS (MenuItems i) r ()
promptChange = \case
  PromptAppend ->
    appendFilter
  PromptUnappend ->
    popFiltered
  PromptRandom ->
    resetFiltered

insertItems ::
  Member MenuFilter r =>
  [MenuItem i] ->
  SemS (MenuItems i) r ()
insertItems new = do
  index <- use #itemCount
  #itemCount += length new
  let newI = IntMap.fromList (zip [index..] new)
  #items %= IntMap.union newI
  query <- use #currentQuery
  ents <- SemS (MenuFilter.initial query newI)
  #entries %= IntMap.unionWith (<>) ents
  unless (null ents) do
    #history .= mempty

promptItemUpdate ::
  Member MenuFilter r =>
  PromptChange ->
  Prompt ->
  SemS (MenuItems i) r ()
promptItemUpdate change (Prompt _ _ (PromptText (MenuQuery -> query))) =
  promptChange change query

diffPrompt :: Prompt -> MenuQuery -> PromptChange
diffPrompt (Prompt _ _ (PromptText new)) (MenuQuery old)
  | Text.isPrefixOf old new = PromptAppend
  | Text.isPrefixOf new old = PromptUnappend
  | otherwise = PromptRandom

queryUpdate ::
  Members [MenuState i, MenuFilter] r =>
  Sem r MenuEvent
queryUpdate = do
  useItems \ s -> do
    prompt <- readPrompt
    runState s $ semState do
      change <- use (#currentQuery . to (diffPrompt prompt))
      promptItemUpdate change prompt
      pure MenuEvent.PromptEdit

classifyEvent :: PromptEvent -> Maybe MenuEvent
classifyEvent = \case
  PromptEvent.Mapping c ->
    Just (MenuEvent.Mapping c)
  PromptEvent.Edit ->
    Nothing
  PromptEvent.Navigation ->
    Just MenuEvent.PromptNavigation
  PromptEvent.Init ->
    Just MenuEvent.Init
  PromptEvent.Quit ->
    Just (MenuEvent.Quit QuitReason.Aborted)
  PromptEvent.Error e ->
    Just (MenuEvent.Quit (QuitReason.Error e))

setPromptAndClassify ::
  Member (MenuState i) r =>
  Member Log r =>
  Prompt ->
  PromptEvent ->
  Sem r (Maybe MenuEvent)
setPromptAndClassify prompt event = do
  Log.debug [exon|prompt event: #{show @Text event}|]
  classifyEvent event <$ setPrompt prompt
