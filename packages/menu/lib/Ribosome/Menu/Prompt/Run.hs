module Ribosome.Menu.Prompt.Run where

import Conc (interpretSyncAs, withAsync_)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMChan (TMChan, closeTMChan, newTMChan)
import Control.Lens ((^.))
import qualified Data.Text as Text (drop, dropEnd, isPrefixOf, length, splitAt)
import Exon (exon)
import qualified Log
import Prelude hiding (input, modifyMVar, output)
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Prelude (SerialT)
import qualified Sync
import Time (MilliSeconds, convert)

import Ribosome.Api.Input (syntheticInput)
import Ribosome.Final (inFinal)
import Ribosome.Host.Data.Tuple (dup)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Menu.Prompt.Data.CursorUpdate (CursorUpdate)
import qualified Ribosome.Menu.Prompt.Data.CursorUpdate as CursorUpdate (CursorUpdate (..))
import qualified Ribosome.Menu.Prompt.Data.Prompt as Prompt
import Ribosome.Menu.Prompt.Data.Prompt (
  Prompt (Prompt),
  PromptChange (PromptAppend, PromptRandom, PromptUnappend),
  PromptText (PromptText),
  )
import qualified Ribosome.Menu.Prompt.Data.PromptConfig as PromptConfig
import Ribosome.Menu.Prompt.Data.PromptConfig (
  PromptConfig (PromptConfig),
  PromptEventHandler (PromptEventHandler),
  PromptInput (PromptInput),
  PromptListening (PromptListening),
  startInsert,
  )
import qualified Ribosome.Menu.Prompt.Data.PromptControlEvent as PromptControlEvent
import Ribosome.Menu.Prompt.Data.PromptControlEvent (PromptControlEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import Ribosome.Menu.Prompt.Data.PromptInputEvent (PromptInputEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptInputEvent as PromptInputEvent (PromptInputEvent (..))
import Ribosome.Menu.Prompt.Data.PromptRenderer (PromptRenderer (PromptRenderer), render)
import Ribosome.Menu.Prompt.Data.PromptState (PromptState)
import qualified Ribosome.Menu.Prompt.Data.PromptState as PromptState (PromptState (..))
import qualified Ribosome.Menu.Prompt.Data.PromptUpdate as PromptUpdate
import Ribosome.Menu.Prompt.Data.PromptUpdate (PromptUpdate)
import Ribosome.Menu.Prompt.Data.TextUpdate (TextUpdate)
import qualified Ribosome.Menu.Prompt.Data.TextUpdate as TextUpdate (TextUpdate (..))
import Ribosome.Menu.Stream.Util (chanStream, takeUntilNothing)

updateCursor :: Int -> Text -> CursorUpdate -> Int
updateCursor current text =
  update
  where
    update CursorUpdate.OneLeft | current > 0 =
      current - 1
    update CursorUpdate.OneLeft =
      current
    update CursorUpdate.OneRight | current <= textLength =
      current + 1
    update CursorUpdate.OneRight =
      current
    update CursorUpdate.Prepend =
      0
    update CursorUpdate.Append =
      Text.length text + 1
    update (CursorUpdate.Index index) =
      min textLength (max 0 index)
    update CursorUpdate.Unmodified =
      current
    textLength =
      Text.length text

updateText :: Int -> Text -> TextUpdate -> Text
updateText cursor text =
  update
  where
    update TextUpdate.Unmodified =
      text
    update (TextUpdate.Insert new) =
      pre <> new <> post
    update TextUpdate.DeleteLeft =
      Text.dropEnd 1 pre <> post
    update TextUpdate.DeleteRight =
      pre <> Text.drop 1 post
    update (TextUpdate.Set newText) =
      newText
    (pre, post) =
      Text.splitAt cursor text

updateLastChange ::
  Text ->
  Text ->
  PromptChange
updateLastChange old new
  | Text.length old + 1 == Text.length new && Text.isPrefixOf old new = PromptAppend
  | Text.length new + 1 == Text.length old && Text.isPrefixOf new old = PromptUnappend
  | otherwise = PromptRandom

modifyPrompt :: PromptState -> CursorUpdate -> TextUpdate -> Prompt -> Prompt
modifyPrompt newState cursorUpdate textUpdate (Prompt cursor _ (PromptText text)) =
  Prompt updatedCursor newState (PromptText updatedText)
  where
    updatedCursor =
      updateCursor cursor updatedText cursorUpdate
    updatedText =
      updateText cursor text textUpdate

modifyEvent ::
  TextUpdate ->
  PromptEvent
modifyEvent = \case
  TextUpdate.Unmodified ->
    PromptEvent.Navigation
  _ ->
    PromptEvent.Edit

ignoreEvent :: PromptInputEvent -> PromptEvent
ignoreEvent = \case
  PromptInputEvent.Init ->
    PromptEvent.Init
  PromptInputEvent.Character c ->
    PromptEvent.Mapping c
  PromptInputEvent.Error err ->
    PromptEvent.Error err
  PromptInputEvent.Interrupt ->
    PromptEvent.Quit

updatePrompt ::
  Monad m =>
  (PromptInputEvent -> PromptState -> m PromptUpdate) ->
  PromptInputEvent ->
  Prompt ->
  m (Maybe (Prompt, PromptEvent))
updatePrompt handleEvent input prompt = do
  handleEvent input (prompt ^. Prompt.state) <&> \case
    PromptUpdate.Modify newState cursorUpdate textUpdate -> do
      Just (modifyPrompt newState cursorUpdate textUpdate prompt, modifyEvent textUpdate)
    PromptUpdate.Ignore ->
      Just (prompt, ignoreEvent input)
    PromptUpdate.Quit ->
      Nothing

logPromptUpdate ::
  Member Log r =>
  PromptInputEvent ->
  (Prompt, Maybe (Prompt, PromptEvent)) ->
  Sem r ()
logPromptUpdate event res =
  Log.debug [exon|prompt input event: #{show event} -> #{result res}|]
  where
    result = \case
      (new, Just _) -> show new
      (_, Nothing) -> "unhandled"

updatePromptState ::
  Member Log r =>
  (PromptInputEvent -> PromptState -> Sem r PromptUpdate) ->
  PromptInputEvent ->
  Prompt ->
  Sem r (Prompt, Maybe (Prompt, PromptEvent))
updatePromptState handleEvent input old = do
  res <- updatePrompt handleEvent input old <&> \case
    Just (new, output) ->
      (new, Just (new, output))
    Nothing ->
      (old, Nothing)
  res <$ logPromptUpdate input res

processPromptEvent ::
  Members [Sync Prompt, Mask res, Log, Resource] r =>
  PromptEventHandler r ->
  Either PromptControlEvent PromptInputEvent ->
  Sem r (Maybe (Prompt, PromptEvent))
processPromptEvent (PromptEventHandler handleEvent) = \case
  Right event ->
    Sync.modify (updatePromptState handleEvent event)
  Left PromptControlEvent.Quit ->
    pure Nothing
  Left (PromptControlEvent.Set (Prompt cursor state (PromptText text))) -> do
    newPrompt <- Sync.modify (pure . dup . modifyPrompt state (CursorUpdate.Index cursor) (TextUpdate.Set text))
    pure (Just (newPrompt, PromptEvent.Edit))

promptWithControl ::
  Members [Sync Prompt, Sync PromptListening, Mask res, Log, Resource] r =>
  (∀ x . Sem r x -> IO (Maybe x)) ->
  PromptConfig r ->
  SerialT IO PromptControlEvent ->
  MVar () ->
  SerialT IO (Prompt, PromptEvent)
promptWithControl lower (PromptConfig (PromptInput source) handler renderer flags) control listenQuit =
  Stream.tapAsync (Fold.drainBy (lower . render renderer . fst)) $
  takeUntilNothing $
  Stream.mapM (liftIO . fmap join . lower . processPromptEvent (handler flags)) $
  Stream.parallelMin (Left <$> Stream.before (lower (Sync.putTry PromptListening)) control) (Right <$> sourceWithInit)
  where
    sourceWithInit =
      Stream.cons PromptInputEvent.Init (source listenQuit)

controlChannel ::
  Member (Embed IO) r =>
  Sem r (TMChan a, MVar (), IO (), SerialT IO a)
controlChannel = do
  listenQuit <- embed newEmptyMVar
  chan <- embed (atomically newTMChan)
  pure (chan, listenQuit, putMVar listenQuit () *> atomically (closeTMChan chan), chanStream chan)

pristinePrompt :: Bool -> Prompt
pristinePrompt insert =
  Prompt 0 (if insert then PromptState.Insert else PromptState.Normal) ""

withPromptStream ::
  Members [Mask res, Sync PromptListening, Log, Resource, Race, Embed IO, Final IO] r =>
  PromptConfig (Sync Prompt : r) ->
  ((TMChan PromptControlEvent, SerialT IO (Prompt, PromptEvent)) -> Sem r a) ->
  Sem r a
withPromptStream config use = do
  (chan, listenQuit, close, control) <- controlChannel
  interpretSyncAs (pristinePrompt (startInsert (config ^. PromptConfig.flags))) do
    res <- inFinal \ _ lower pur ex ->
      pur (chan, Stream.finally close (promptWithControl (fmap ex . lower) config control listenQuit))
    raise (use res)

noPromptRenderer ::
  PromptRenderer r
noPromptRenderer =
  PromptRenderer unit (const unit) (const unit)

withPromptInput ::
  Members [Sync PromptListening, Rpc, Resource, Race, Async, Time t d] r =>
  Maybe MilliSeconds ->
  [Text] ->
  Sem r a ->
  Sem r a
withPromptInput interval chrs =
  withAsync_ (Sync.takeBlock *> syntheticInput (convert <$> interval) chrs)
