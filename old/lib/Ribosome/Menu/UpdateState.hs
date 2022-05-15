module Ribosome.Menu.UpdateState where

import Control.Lens (use, uses, (%=), (+=), (.=))
import Control.Monad.Catch (MonadCatch, MonadThrow)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as Text
import qualified Data.Trie as Trie
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Prelude (AsyncT, IsStream)

import Ribosome.Menu.Combinators (push)
import Ribosome.Menu.Data.Entry (Entries, insertFiltered)
import Ribosome.Menu.Data.MenuData (MenuCursor, MenuQuery (MenuQuery), currentQuery, entries, history, itemCount, items)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Data.MenuItemFilter (MenuItemFilter (MenuItemFilter))
import Ribosome.Menu.Data.MenuState (MenuItemsM, MenuState, menuItemsStateT, setPrompt)
import qualified Ribosome.Menu.Data.QuitReason as QuitReason
import Ribosome.Menu.Prompt.Data.Prompt (
  Prompt (Prompt),
  PromptChange (PromptAppend, PromptRandom, PromptUnappend),
  PromptText (PromptText),
  )
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import Ribosome.Stream.Accumulate (mapMAcc)

-- TODO parallelize
refineFiltered ::
  Monad m =>
  MenuQuery ->
  MenuItemFilter i ->
  Entries i ->
  MenuItemsM m i ()
refineFiltered query (MenuItemFilter _ _ itemFilter) ents =
  push query (itemFilter query ents)

resetFiltered ::
  Monad m =>
  MenuQuery ->
  MenuItemFilter i ->
  MenuItemsM m i ()
resetFiltered query (MenuItemFilter _ itemFilter _) = do
  its <- use items
  entries .= (itemFilter query its)
  currentQuery .= query

popFiltered ::
  Monad m =>
  MenuQuery ->
  MenuItemFilter i ->
  MenuItemsM m i ()
popFiltered query@(MenuQuery (encodeUtf8 -> queryBs)) itemFilter =
  maybe (resetFiltered query itemFilter) matching =<< uses history (flip Trie.match queryBs)
  where
    matching = \case
      (_, f, "") -> do
        entries .= f
        currentQuery .= query
      (_, f, _) ->
        refineFiltered query itemFilter f

appendFilter ::
  Monad m =>
  MenuQuery ->
  MenuItemFilter i ->
  MenuItemsM m i ()
appendFilter query filt =
  ifM (uses entries null) (resetFiltered query filt) (refineFiltered query filt =<< use entries)

promptChange ::
  Monad m =>
  PromptChange ->
  MenuQuery ->
  MenuItemFilter i ->
  MenuItemsM m i ()
promptChange = \case
  PromptAppend ->
    appendFilter
  PromptUnappend ->
    popFiltered
  PromptRandom ->
    resetFiltered

-- TODO try and see what happens if all 50k items are inserted at once
insertItem ::
  Monad m =>
  MenuItemFilter i ->
  MenuItem i ->
  Prompt ->
  MenuCursor ->
  MenuItemsM m i ()
insertItem (MenuItemFilter match _ _) item _ _ = do
  index <- use itemCount
  itemCount += 1
  items %= IntMap.insert index item
  MenuQuery query <- use currentQuery
  for_ (match query index item) \ (score, fitem) -> do
    entries %= insertFiltered score fitem
    history .= mempty

promptItemUpdate ::
  Monad m =>
  MenuItemFilter i ->
  PromptChange ->
  Prompt ->
  MenuItemsM m i ()
promptItemUpdate itemFilter change (Prompt _ _ (PromptText (MenuQuery -> query))) =
  promptChange change query itemFilter

diffPrompt :: Prompt -> MenuQuery -> PromptChange
diffPrompt (Prompt _ _ (PromptText new)) (MenuQuery old) =
  if Text.isPrefixOf old new
  then PromptAppend
  else if Text.isPrefixOf new old
  then PromptUnappend
  else PromptRandom

queryUpdate ::
  MonadIO m =>
  MonadBaseControl IO m =>
  MenuState i ->
  MenuItemFilter i ->
  m MenuEvent
queryUpdate menu itemFilter = do
  menuItemsStateT menu \ prompt _ -> do
    change <- uses currentQuery (diffPrompt prompt)
    promptItemUpdate itemFilter change prompt
    pure MenuEvent.PromptEdit

classifyEvent :: PromptEvent -> Either () MenuEvent
classifyEvent = \case
  PromptEvent.Mapping c ->
    Right (MenuEvent.Mapping c)
  PromptEvent.Edit ->
    Left ()
  PromptEvent.Navigation ->
    Right MenuEvent.PromptNavigation
  PromptEvent.Init ->
    Right MenuEvent.Init
  PromptEvent.Quit ->
    Right (MenuEvent.Quit QuitReason.Aborted)
  PromptEvent.Error e ->
    Right (MenuEvent.Quit (QuitReason.Error e))

setPromptAndClassify ::
  MenuState i ->
  Prompt ->
  PromptEvent ->
  m (Either () MenuEvent)
setPromptAndClassify menu prompt event = do
  logDebug [exon|prompt event: #{show @Text event}|]
  classifyEvent event <$ setPrompt menu prompt

promptEvent ::
  MonadCatch m =>
  IsStream stream =>
  MonadBaseControl IO m =>
  MenuState i ->
  MenuItemFilter i ->
  AsyncT m (Prompt, PromptEvent) ->
  stream m MenuEvent
promptEvent menu itemFilter =
  Stream.fromAsync .
  mapMAcc (uncurry (setPromptAndClassify menu)) (const (queryUpdate menu itemFilter)) .
  Stream.mkAsync

updateItems ::
  MonadIO m =>
  MonadThrow m =>
  IsStream stream =>
  MonadBaseControl IO m =>
  MenuState i ->
  MenuItemFilter i ->
  stream m (MenuItem i) ->
  stream m MenuEvent
updateItems menu itemFilter =
  Stream.catMaybes .
  Stream.foldIterateM chunker (pure Nothing) .
  Stream.mapM \ item ->
    MenuEvent.NewItem <$ menuItemsStateT menu (insertItem itemFilter item)
  where
    chunker = pure . \case
      Nothing ->
        Fold.take 100 Fold.last
      Just _ ->
        Fold.take 10000 Fold.last
