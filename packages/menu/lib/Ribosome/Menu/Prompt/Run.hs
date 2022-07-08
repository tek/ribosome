module Ribosome.Menu.Prompt.Run where

import Conc (interpretSyncAs, withAsync_)
import Data.Generics.Labels ()
import qualified Data.Text as Text (drop, dropEnd, isPrefixOf, length, splitAt)
import Exon (exon)
import qualified Log
import qualified Polysemy.Time as Time
import Prelude hiding (input, modifyMVar, output)
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Prelude (SerialT)
import qualified Sync
import Time (MilliSeconds (MilliSeconds), convert)

import Ribosome.Api.Input (syntheticInput)
import Ribosome.Final (inFinal)
import Ribosome.Host.Data.Tuple (dup)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Menu.Effect.PromptControl (PromptControl, controlEvent, quitPrompt, startPrompt, waitPromptListening)
import Ribosome.Menu.Effect.PromptEvents (PromptEvents, handlePromptEvent)
import qualified Ribosome.Menu.Effect.PromptInput as PromptInput
import Ribosome.Menu.Effect.PromptInput (PromptInput)
import qualified Ribosome.Menu.Effect.PromptRenderer as PromptRenderer
import Ribosome.Menu.Effect.PromptRenderer (PromptRenderer)
import Ribosome.Menu.Prompt.Data.CursorUpdate (CursorUpdate)
import qualified Ribosome.Menu.Prompt.Data.CursorUpdate as CursorUpdate (CursorUpdate (..))
import qualified Ribosome.Menu.Prompt.Data.InputEvent as InputEvent
import Ribosome.Menu.Prompt.Data.Prompt (
  Prompt (Prompt),
  PromptChange (PromptAppend, PromptRandom, PromptUnappend),
  PromptText (PromptText),
  )
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
import Ribosome.Menu.Stream.Util (takeUntilNothing)

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

inputEvent ::
  Members [PromptInput, Time t d] r =>
  (∀ x . Sem r x -> IO (Maybe x)) ->
  SerialT IO PromptInputEvent
inputEvent lower =
  spin
  where
    spin =
      Stream.fromEffect (lower PromptInput.event) >>= \case
        Just event -> case event of
          InputEvent.Character a ->
            Stream.cons (PromptInputEvent.Character a) spin
          InputEvent.Interrupt ->
            Stream.fromPure PromptInputEvent.Interrupt
          InputEvent.Error e ->
            Stream.fromPure (PromptInputEvent.Error e)
          InputEvent.NoInput ->
            Stream.before (void (lower (Time.sleep (MilliSeconds 33)))) spin
          InputEvent.Unexpected _ ->
            spin
        Nothing ->
          Stream.nil

promptWithControl ::
  Members [PromptControl, PromptEvents, PromptInput, PromptRenderer, Sync Prompt, Mask res, Time t d, Log, Resource] r =>
  (∀ x . Sem r x -> IO (Maybe x)) ->
  SerialT IO PromptControlEvent ->
  SerialT IO (Prompt, PromptEvent)
promptWithControl lower control =
  Stream.tapAsync (Fold.drainBy (lower . PromptRenderer.renderPrompt . fst)) $
  takeUntilNothing $
  Stream.mapM (liftIO . fmap join . lower . processPromptEvent) $
  Stream.parallelMin (Left <$> Stream.before (lower startPrompt) control) (Right <$> sourceWithInit)
  where
    sourceWithInit =
      Stream.cons PromptInputEvent.Init (inputEvent lower)

pristinePrompt :: Bool -> Prompt
pristinePrompt insert =
  Prompt 0 (if insert then PromptMode.Insert else PromptMode.Normal) ""

withPromptStream ::
  Members [PromptControl, PromptEvents, PromptInput, PromptRenderer] r =>
  Members [Mask res, Time t d, Log, Resource, Race, Embed IO, Final IO] r =>
  Prompt ->
  (SerialT IO (Prompt, PromptEvent) -> Sem r a) ->
  Sem r a
withPromptStream initial use = do
  -- chan <- embed (atomically newTMChan)
  interpretSyncAs initial do
    res <- inFinal \ _ lower pur ex -> do
      let
        backchannel =
          takeUntilNothing (Stream.repeatM ((join . ex) <$> lower controlEvent))
      pur (Stream.finally (lower quitPrompt) (promptWithControl (fmap ex . lower) backchannel))
    raise (use res)

withPromptInput ::
  Members [PromptControl, Rpc, Resource, Race, Async, Time t d] r =>
  Maybe MilliSeconds ->
  [Text] ->
  Sem r a ->
  Sem r a
withPromptInput interval chrs =
  withAsync_ (waitPromptListening *> syntheticInput (convert <$> interval) chrs)
