module Ribosome.Menu.Prompt.Run where

import Control.Concurrent.Lifted (modifyMVar)
import Control.Concurrent.STM.TMChan (TMChan, closeTMChan, newTMChan)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import qualified Data.Text as Text (drop, dropEnd, isPrefixOf, length, splitAt)
import Prelude hiding (state)
import Relude.Extra (dup)
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Prelude (SerialT)

import Ribosome.Control.Monad.Ribo (MonadRibo)
import Ribosome.Data.Stream (chanStream, takeUntilNothing)
import Ribosome.Log (logDebug)
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
updateLastChange old new =
  if Text.length old + 1 == Text.length new && Text.isPrefixOf old new
  then PromptAppend
  else if Text.length new + 1 == Text.length old && Text.isPrefixOf new old
  then PromptUnappend
  else PromptRandom

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

updatePromptState ::
  Monad m =>
  MonadRibo m =>
  (PromptInputEvent -> PromptState -> m PromptUpdate) ->
  PromptInputEvent ->
  Prompt ->
  m (Prompt, Maybe (Prompt, PromptEvent))
updatePromptState handleEvent input old = do
  logDebug @Text [exon|prompt input event: #{show input}|]
  updatePrompt handleEvent input old <&> \case
    Just (new, output) ->
      (new, Just (new, output))
    Nothing ->
      (old, Nothing)

processPromptEvent ::
  MonadRibo m =>
  MonadBaseControl IO m =>
  MVar Prompt ->
  PromptEventHandler m ->
  Either PromptControlEvent PromptInputEvent ->
  m (Maybe (Prompt, PromptEvent))
processPromptEvent prompt (PromptEventHandler handleEvent) = \case
  Right event ->
    modifyMVar prompt (updatePromptState handleEvent event)
  Left PromptControlEvent.Quit ->
    pure Nothing
  Left (PromptControlEvent.Set (Prompt cursor state (PromptText text))) -> do
    newPrompt <- modifyMVar prompt (pure . dup . modifyPrompt state (CursorUpdate.Index cursor) (TextUpdate.Set text))
    pure(Just (newPrompt, PromptEvent.Edit))

promptWithControl ::
  MonadRibo m =>
  MonadCatch m =>
  MonadBaseControl IO m =>
  MVar Prompt ->
  PromptConfig m ->
  SerialT m PromptControlEvent ->
  MVar () ->
  SerialT m (Prompt, PromptEvent)
promptWithControl prompt (PromptConfig (PromptInput source) handler renderer flags) control listenQuit =
  Stream.tapAsync (Fold.drainBy (render renderer . fst)) $
  takeUntilNothing $
  Stream.mapM (processPromptEvent prompt (handler flags)) $
  Stream.parallelMin (Left <$> control) (Right <$> sourceWithInit)
  where
    sourceWithInit =
      Stream.cons PromptInputEvent.Init (source listenQuit)

controlChannel ::
  MonadIO m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  m (TMChan a, MVar (), m (), SerialT m a)
controlChannel = do
  listenQuit <- newEmptyMVar
  chan <- atomically newTMChan
  pure (chan, listenQuit, putMVar listenQuit () *> atomically (closeTMChan chan), chanStream chan)

promptStream ::
  MonadRibo m =>
  MonadCatch m =>
  MonadBaseControl IO m =>
  PromptConfig m ->
  m (TMChan PromptControlEvent, SerialT m (Prompt, PromptEvent))
promptStream config = do
  (chan, listenQuit, close, control) <- controlChannel
  prompt <- newMVar (pristinePrompt (startInsert (config ^. PromptConfig.flags)))
  pure (chan, Stream.finally close (promptWithControl prompt config control listenQuit))

pristinePrompt :: Bool -> Prompt
pristinePrompt insert =
  Prompt 0 (if insert then PromptState.Insert else PromptState.Normal) ""

noPromptRenderer ::
  Applicative m =>
  PromptRenderer m
noPromptRenderer =
  PromptRenderer unit (const unit) (const unit)
