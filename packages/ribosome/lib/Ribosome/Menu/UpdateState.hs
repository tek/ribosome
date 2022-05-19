module Ribosome.Menu.UpdateState where

import Control.Lens (use, uses, (%=), (+=), (.=))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as Text
import qualified Data.Trie as Trie
import Exon (exon)
import qualified Polysemy.Log as Log
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Prelude (AsyncT, IsStream, SerialT)

import Ribosome.Menu.Combinators (push)
import Ribosome.Menu.Data.Entry (Entries, insertFiltered)
import Ribosome.Menu.Data.MenuData (MenuCursor, MenuQuery (MenuQuery), currentQuery, entries, history, itemCount, items)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuItemFilter (MenuItemFilter (MenuItemFilter))
import Ribosome.Menu.Data.MenuStateSem (
  CursorLock,
  ItemsLock,
  MenuItemsSem,
  MenuItemsSemS,
  MenuState,
  menuItemsStateSem,
  semState,
  setPrompt,
  )
import qualified Ribosome.Menu.Data.QuitReason as QuitReason
import Ribosome.Menu.Prompt.Data.Prompt (
  Prompt (Prompt),
  PromptChange (PromptAppend, PromptRandom, PromptUnappend),
  PromptText (PromptText),
  )
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import Ribosome.Stream.Accumulate (mapMAccMaybe)

-- TODO parallelize
refineFiltered ::
  MenuQuery ->
  MenuItemFilter i ->
  Entries i ->
  MenuItemsSemS r i ()
refineFiltered query (MenuItemFilter _ _ itemFilter) ents =
  push query (itemFilter query ents)

resetFiltered ::
  MenuQuery ->
  MenuItemFilter i ->
  MenuItemsSemS r i ()
resetFiltered query (MenuItemFilter _ itemFilter _) = do
  its <- use items
  entries .= itemFilter query its
  currentQuery .= query

popFiltered ::
  MenuQuery ->
  MenuItemFilter i ->
  MenuItemsSemS r i ()
popFiltered query@(MenuQuery (encodeUtf8 -> queryBs)) itemFilter =
  maybe (resetFiltered query itemFilter) matching =<< uses history (`Trie.match` queryBs)
  where
    matching = \case
      (_, f, "") -> do
        entries .= f
        currentQuery .= query
      (_, f, _) ->
        refineFiltered query itemFilter f

appendFilter ::
  MenuQuery ->
  MenuItemFilter i ->
  MenuItemsSemS r i ()
appendFilter query filt =
  ifM (uses entries null) (resetFiltered query filt) (refineFiltered query filt =<< use entries)

promptChange ::
  PromptChange ->
  MenuQuery ->
  MenuItemFilter i ->
  MenuItemsSemS r i ()
promptChange = \case
  PromptAppend ->
    appendFilter
  PromptUnappend ->
    popFiltered
  PromptRandom ->
    resetFiltered

-- TODO try and see what happens if all 50k items are inserted at once
insertItem ::
  MenuItemFilter i ->
  MenuItem i ->
  Prompt ->
  MenuCursor ->
  MenuItemsSem r i ()
insertItem (MenuItemFilter match _ _) item _ _ =
  semState do
    index <- use itemCount
    itemCount += 1
    items %= IntMap.insert index item
    MenuQuery query <- use currentQuery
    for_ (match query index item) \ (score, fitem) -> do
      entries %= insertFiltered score fitem
      history .= mempty

promptItemUpdate ::
  MenuItemFilter i ->
  PromptChange ->
  Prompt ->
  MenuItemsSemS r i ()
promptItemUpdate itemFilter change (Prompt _ _ (PromptText (MenuQuery -> query))) =
  promptChange change query itemFilter

diffPrompt :: Prompt -> MenuQuery -> PromptChange
diffPrompt (Prompt _ _ (PromptText new)) (MenuQuery old)
  | Text.isPrefixOf old new = PromptAppend
  | Text.isPrefixOf new old = PromptUnappend
  | otherwise = PromptRandom

queryUpdate ::
  Members [Resource, Embed IO] r =>
  MenuState i ->
  MenuItemFilter i ->
  Sem (Sync ItemsLock : Sync CursorLock : r) MenuEvent
queryUpdate menu itemFilter =
  menuItemsStateSem menu \ prompt _ ->
    semState do
      change <- uses currentQuery (diffPrompt prompt)
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
  Member (Embed IO) r =>
  Member Log r =>
  MenuState i ->
  Prompt ->
  PromptEvent ->
  Sem r (Maybe MenuEvent)
setPromptAndClassify menu prompt event = do
  Log.debug [exon|prompt event: #{show @Text event}|]
  classifyEvent event <$ setPrompt menu prompt

promptEvent ::
  Members [Sync ItemsLock, Sync CursorLock, Log, Resource, Embed IO] r =>
  (∀ x . Sem r x -> IO (Maybe x)) ->
  MenuState i ->
  MenuItemFilter i ->
  AsyncT IO (Prompt, PromptEvent) ->
  SerialT IO MenuEvent
promptEvent lower menu itemFilter str =
  Stream.fromAsync $
  mapMAccMaybe (fmap join . lower . uncurry (setPromptAndClassify menu)) (fromMaybe MenuEvent.PromptEdit <$> lower (subsume (subsume (queryUpdate menu itemFilter)))) $
  Stream.mkAsync str

updateItems ::
  IsStream t =>
  Functor (t IO) =>
  Members [Sync ItemsLock, Sync CursorLock, Log, Resource, Embed IO] r =>
  (∀ x . Sem r x -> IO (Maybe x)) ->
  MenuState i ->
  MenuItemFilter i ->
  t IO (MenuItem i) ->
  t IO MenuEvent
updateItems lower menu itemFilter =
  Stream.catMaybes .
  Stream.foldIterateM chunker (pure Nothing) .
  Stream.mapM \ item ->
    MenuEvent.NewItem <$ lower (subsume (subsume (menuItemsStateSem menu (insertItem itemFilter item))))
  where
    chunker = pure . \case
      Nothing ->
        Fold.take 100 Fold.last
      Just _ ->
        Fold.take 10000 Fold.last
