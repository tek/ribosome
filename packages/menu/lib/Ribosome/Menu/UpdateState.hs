module Ribosome.Menu.UpdateState where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as Text
import qualified Data.Trie as Trie
import Exon (exon)
import Lens.Micro.Mtl (use, (%=), (+=), (.=))
import qualified Polysemy.Log as Log
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Prelude (AsyncT, IsStream, SerialT)

import Ribosome.Menu.Combinators (push)
import Ribosome.Menu.Data.Entry (Entries)
import Ribosome.Menu.Data.MenuData (MenuItems, MenuQuery (MenuQuery))
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuItemFilter (MenuItemFilter (MenuItemFilter))
import Ribosome.Menu.Data.MenuState (SemS (SemS), semState)
import qualified Ribosome.Menu.Data.QuitReason as QuitReason
import Ribosome.Menu.Effect.MenuState (MenuState, readPrompt, setPrompt, useItems)
import Ribosome.Menu.Prompt.Data.Prompt (
  Prompt (Prompt),
  PromptChange (PromptAppend, PromptRandom, PromptUnappend),
  PromptText (PromptText),
  )
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import Ribosome.Menu.Stream.Accumulate (mapMAccMaybe)

-- TODO parallelize
refineFiltered ::
  Member (Embed IO) r =>
  MenuQuery ->
  MenuItemFilter i ->
  Entries i ->
  SemS (MenuItems i) r ()
refineFiltered query (MenuItemFilter _ _ itemFilter) ents =
  push query =<< SemS (embed (itemFilter query ents))

resetFiltered ::
  Member (Embed IO) r =>
  MenuQuery ->
  MenuItemFilter i ->
  SemS (MenuItems i) r ()
resetFiltered query (MenuItemFilter _ itemFilter _) = do
  its <- use #items
  new <- SemS (embed (itemFilter query its))
  #entries .= new
  #currentQuery .= query

popFiltered ::
  Member (Embed IO) r =>
  MenuQuery ->
  MenuItemFilter i ->
  SemS (MenuItems i) r ()
popFiltered query@(MenuQuery (encodeUtf8 -> queryBs)) itemFilter =
  maybe (resetFiltered query itemFilter) matching =<< use (#history . to (`Trie.match` queryBs))
  where
    matching = \case
      (_, f, "") -> do
        #entries .= f
        #currentQuery .= query
      (_, f, _) ->
        refineFiltered query itemFilter f

appendFilter ::
  Member (Embed IO) r =>
  MenuQuery ->
  MenuItemFilter i ->
  SemS (MenuItems i) r ()
appendFilter query filt =
  ifM (use (#entries . to null)) (resetFiltered query filt) (refineFiltered query filt =<< use #entries)

promptChange ::
  Member (Embed IO) r =>
  PromptChange ->
  MenuQuery ->
  MenuItemFilter i ->
  SemS (MenuItems i) r ()
promptChange = \case
  PromptAppend ->
    appendFilter
  PromptUnappend ->
    popFiltered
  PromptRandom ->
    resetFiltered

insertItems ::
  Member (Embed IO) r =>
  MenuItemFilter i ->
  [MenuItem i] ->
  SemS (MenuItems i) r ()
insertItems (MenuItemFilter _ filt _) new = do
  index <- use #itemCount
  #itemCount += length new
  let newI = IntMap.fromList (zip [index..] new)
  #items %= IntMap.union newI
  query <- use #currentQuery
  ents <- SemS (embed (filt query newI))
  #entries %= IntMap.unionWith (<>) ents
  unless (null ents) do
    #history .= mempty

promptItemUpdate ::
  Member (Embed IO) r =>
  MenuItemFilter i ->
  PromptChange ->
  Prompt ->
  SemS (MenuItems i) r ()
promptItemUpdate itemFilter change (Prompt _ _ (PromptText (MenuQuery -> query))) =
  promptChange change query itemFilter

diffPrompt :: Prompt -> MenuQuery -> PromptChange
diffPrompt (Prompt _ _ (PromptText new)) (MenuQuery old)
  | Text.isPrefixOf old new = PromptAppend
  | Text.isPrefixOf new old = PromptUnappend
  | otherwise = PromptRandom

queryUpdate ::
  Members [MenuState i, Resource, Embed IO] r =>
  MenuItemFilter i ->
  Sem r MenuEvent
queryUpdate itemFilter = do
  useItems \ s -> do
    prompt <- readPrompt
    runState s $ semState do
      change <- use (#currentQuery . to (diffPrompt prompt))
      promptItemUpdate itemFilter change prompt
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
  Members [MenuState i, Embed IO] r =>
  Member Log r =>
  Prompt ->
  PromptEvent ->
  Sem r (Maybe MenuEvent)
setPromptAndClassify prompt event = do
  Log.debug [exon|prompt event: #{show @Text event}|]
  classifyEvent event <$ setPrompt prompt

promptEvent ::
  Members [MenuState i, Log, Resource, Embed IO] r =>
  (∀ x . Sem r x -> IO (Maybe x)) ->
  MenuItemFilter i ->
  AsyncT IO (Prompt, PromptEvent) ->
  SerialT IO MenuEvent
promptEvent lower itemFilter str =
  Stream.fromAsync $
  mapMAccMaybe (fmap join . lower . uncurry setPromptAndClassify) (fromMaybe MenuEvent.PromptEdit <$> lower (queryUpdate itemFilter)) $
  Stream.mkAsync str

updateItems ::
  IsStream t =>
  Members [MenuState i, Log, Embed IO] r =>
  (∀ x . Sem r x -> IO (Maybe x)) ->
  MenuItemFilter i ->
  t IO (MenuItem i) ->
  t IO MenuEvent
updateItems lower itemFilter =
  flip Stream.serial (Stream.fromPure MenuEvent.Exhausted) .
  Stream.mapM insert .
  Stream.foldIterateM chunker (pure [])
  where
    insert new =
      MenuEvent.NewItems <$ lower (useItems \ items -> runState items (semState (insertItems itemFilter new)))
    chunker = pure . \case
      [] ->
        Fold.take 100 Fold.toList
      _ ->
        Fold.take 10000 Fold.toList
