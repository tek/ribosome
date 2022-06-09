module Ribosome.Menu.Prompt.Run where

import Conc (interpretSyncAs, withAsync_)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMChan (TMChan, closeTMChan, newTMChan)
import Control.Lens ((^.))
import Data.Generics.Labels ()
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
import Ribosome.Menu.Effect.PromptEvents (PromptEvents, handlePromptEvent)
import qualified Ribosome.Menu.Effect.PromptRenderer as PromptRenderer
import Ribosome.Menu.Effect.PromptRenderer (PromptRenderer)
import Ribosome.Menu.Prompt.Data.CursorUpdate (CursorUpdate)
import qualified Ribosome.Menu.Prompt.Data.CursorUpdate as CursorUpdate (CursorUpdate (..))
import Ribosome.Menu.Prompt.Data.Prompt (
  Prompt (Prompt),
  PromptChange (PromptAppend, PromptRandom, PromptUnappend),
  PromptText (PromptText),
  )
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptInput (PromptInput), PromptListening (PromptListening))
import qualified Ribosome.Menu.Prompt.Data.PromptControlEvent as PromptControlEvent
import Ribosome.Menu.Prompt.Data.PromptControlEvent (PromptControlEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import Ribosome.Menu.Prompt.Data.PromptInputEvent (PromptInputEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptInputEvent as PromptInputEvent (PromptInputEvent (..))
import Ribosome.Menu.Prompt.Data.PromptMode (PromptMode)
import qualified Ribosome.Menu.Prompt.Data.PromptMode as PromptMode (PromptMode (..))
import qualified Ribosome.Menu.Prompt.Data.PromptUpdate as PromptUpdate
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

modifyPrompt :: PromptMode -> CursorUpdate -> TextUpdate -> Prompt -> Prompt
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
  Member PromptEvents r =>
  PromptInputEvent ->
  Prompt ->
  Sem r (Maybe (Prompt, PromptEvent))
updatePrompt input prompt = do
  handlePromptEvent input (prompt ^. #state) <&> \case
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
  Members [PromptEvents, Log] r =>
  PromptInputEvent ->
  Prompt ->
  Sem r (Prompt, Maybe (Prompt, PromptEvent))
updatePromptState input old = do
  res <- updatePrompt input old <&> \case
    Just (new, output) ->
      (new, Just (new, output))
    Nothing ->
      (old, Nothing)
  res <$ logPromptUpdate input res

processPromptEvent ::
  Members [PromptEvents, Sync Prompt, Mask res, Log, Resource] r =>
  Either PromptControlEvent PromptInputEvent ->
  Sem r (Maybe (Prompt, PromptEvent))
processPromptEvent = \case
  Right event ->
    Sync.modify (updatePromptState event)
  Left PromptControlEvent.Quit ->
    pure Nothing
  Left (PromptControlEvent.Set (Prompt cursor state (PromptText text))) -> do
    newPrompt <- Sync.modify (pure . dup . modifyPrompt state (CursorUpdate.Index cursor) (TextUpdate.Set text))
    pure (Just (newPrompt, PromptEvent.Edit))

promptWithControl ::
  Members [PromptEvents, PromptRenderer, Sync Prompt, Sync PromptListening, Mask res, Log, Resource] r =>
  (∀ x . Sem r x -> IO (Maybe x)) ->
  PromptInput ->
  SerialT IO PromptControlEvent ->
  MVar () ->
  SerialT IO (Prompt, PromptEvent)
promptWithControl lower (PromptInput source) control listenQuit =
  Stream.tapAsync (Fold.drainBy (lower . PromptRenderer.renderPrompt . fst)) $
  takeUntilNothing $
  Stream.mapM (liftIO . fmap join . lower . processPromptEvent) $
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
  Prompt 0 (if insert then PromptMode.Insert else PromptMode.Normal) ""

withPromptStream ::
  Members [PromptEvents, PromptRenderer, Mask res, Sync PromptListening, Log, Resource, Race, Embed IO, Final IO] r =>
  Prompt ->
  PromptInput ->
  ((TMChan PromptControlEvent, SerialT IO (Prompt, PromptEvent)) -> Sem r a) ->
  Sem r a
withPromptStream initial promptInput use = do
  (chan, listenQuit, close, control) <- controlChannel
  interpretSyncAs initial do
    res <- inFinal \ _ lower pur ex ->
      pur (chan, Stream.finally close (promptWithControl (fmap ex . lower) promptInput control listenQuit))
    raise (use res)

withPromptInput ::
  Members [Sync PromptListening, Rpc, Resource, Race, Async, Time t d] r =>
  Maybe MilliSeconds ->
  [Text] ->
  Sem r a ->
  Sem r a
withPromptInput interval chrs =
  withAsync_ (Sync.takeBlock *> syntheticInput (convert <$> interval) chrs)
