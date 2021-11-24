module Ribosome.Menu.Prompt.Run where

import Control.Concurrent.STM.TMChan (TMChan, closeTMChan, newTMChan)
import Control.Monad.Catch (MonadCatch)
import qualified Data.Text as Text (drop, dropEnd, isPrefixOf, length, splitAt)
import Prelude hiding (state)
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.IsStream as Streamly
import Streamly.Prelude (SerialT, parallel)

import Ribosome.Control.Monad.Ribo (MonadRibo)
import Ribosome.Data.Stream (chanStream)
import Ribosome.Log (logDebug)
import Ribosome.Menu.Prompt.Data.CursorUpdate (CursorUpdate)
import qualified Ribosome.Menu.Prompt.Data.CursorUpdate as CursorUpdate (CursorUpdate (..))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt), PromptChange (PromptAppend, PromptRandom, PromptUnappend))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig (PromptConfig), PromptFlag, onlyInsert, startInsert)
import Ribosome.Menu.Prompt.Data.PromptConsumed (PromptConsumed)
import qualified Ribosome.Menu.Prompt.Data.PromptConsumed as PromptConsumed (PromptConsumed (..))
import qualified Ribosome.Menu.Prompt.Data.PromptConsumerUpdate as PromptConsumerUpdate
import Ribosome.Menu.Prompt.Data.PromptConsumerUpdate (PromptConsumerUpdate (PromptConsumerUpdate))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent (..))
import Ribosome.Menu.Prompt.Data.PromptRenderer (PromptRenderer (PromptRenderer))
import Ribosome.Menu.Prompt.Data.PromptState (PromptState)
import qualified Ribosome.Menu.Prompt.Data.PromptState as PromptState (PromptState (..))
import Ribosome.Menu.Prompt.Data.PromptUpdate (PromptUpdate (PromptUpdate))
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
    (pre, post) = Text.splitAt cursor text

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

updatePrompt ::
  Monad m =>
  (PromptEvent -> PromptState -> m PromptUpdate) ->
  PromptEvent ->
  Prompt ->
  m (PromptConsumed, Prompt)
updatePrompt modes update (Prompt cursor state text _) = do
  (PromptUpdate newState cursorUpdate textUpdate consumed) <- modes update state
  let
    updatedText =
      updateText cursor text textUpdate
    newPrompt =
      Prompt (updateCursor cursor updatedText cursorUpdate) newState updatedText (updateLastChange text updatedText)
  pure (consumed, newPrompt)

processPromptEvent ::
  MonadIO m =>
  MonadRibo m =>
  PromptConfig m ->
  PromptEvent ->
  StateT Prompt m PromptConsumerUpdate
processPromptEvent (PromptConfig _ modes _ flags) event = do
  logDebug @Text $ "prompt event: " <> show event
  consumed <- stateM $ lift . updatePrompt (modes flags) event
  newPrompt <- get
  pure (PromptConsumerUpdate event newPrompt consumed)
  -- lift . lift . render $ newPrompt

promptWithBackchannel ::
  MonadRibo m =>
  MonadCatch m =>
  MonadBaseControl IO m =>
  PromptConfig m ->
  TMChan PromptEvent ->
  SerialT m PromptConsumerUpdate
promptWithBackchannel config@(PromptConfig source _ (PromptRenderer _ _ render) _) backchannel =
  Streamly.tapAsync (Fold.drainBy (render . PromptConsumerUpdate._prompt)) $
  process $
  Streamly.liftInner $
  parallel sourceWithInit (chanStream backchannel)
  where
    sourceWithInit =
      Streamly.finally (atomically (closeTMChan backchannel)) (Streamly.cons PromptEvent.Init source)
    process =
      Streamly.evalStateT (pure (pristinePrompt (startInsert config))) . Streamly.mapM (processPromptEvent config)

promptStream ::
  MonadRibo m =>
  MonadCatch m =>
  MonadBaseControl IO m =>
  PromptConfig m ->
  m (TMChan PromptEvent, SerialT m PromptConsumerUpdate)
promptStream config = do
  chan <- atomically newTMChan
  pure (chan, Streamly.finally (atomically (closeTMChan chan)) (promptWithBackchannel config chan))

unprocessableChars :: [Text]
unprocessableChars =
  [
    "cr",
    "tab"
    ]

unprocessable :: Text -> Bool
unprocessable char =
  char `elem` unprocessableChars || Text.isPrefixOf "c-" char

consumeUnmodified :: PromptState -> CursorUpdate -> PromptUpdate
consumeUnmodified s u =
  PromptUpdate s u TextUpdate.Unmodified PromptConsumed.Yes

basicTransitionNormal ::
  PromptEvent ->
  PromptUpdate
basicTransitionNormal (PromptEvent.Character "esc") =
  consumeUnmodified PromptState.Quit CursorUpdate.Unmodified
basicTransitionNormal (PromptEvent.Character "q") =
  consumeUnmodified PromptState.Quit CursorUpdate.Unmodified
basicTransitionNormal (PromptEvent.Character "i") =
  consumeUnmodified PromptState.Insert CursorUpdate.Unmodified
basicTransitionNormal (PromptEvent.Character "I") =
  consumeUnmodified PromptState.Insert CursorUpdate.Prepend
basicTransitionNormal (PromptEvent.Character "a") =
  consumeUnmodified PromptState.Insert CursorUpdate.OneRight
basicTransitionNormal (PromptEvent.Character "A") =
  consumeUnmodified PromptState.Insert CursorUpdate.Append
basicTransitionNormal (PromptEvent.Character "h") =
  consumeUnmodified PromptState.Normal CursorUpdate.OneLeft
basicTransitionNormal (PromptEvent.Character "l") =
  consumeUnmodified PromptState.Normal CursorUpdate.OneRight
basicTransitionNormal (PromptEvent.Character "x") =
  PromptUpdate PromptState.Normal CursorUpdate.OneLeft TextUpdate.DeleteRight PromptConsumed.Yes
basicTransitionNormal _ =
  PromptUpdate PromptState.Normal CursorUpdate.Unmodified TextUpdate.Unmodified PromptConsumed.No

basicTransitionInsert ::
  [PromptFlag] ->
  PromptEvent ->
  PromptUpdate
basicTransitionInsert flags =
  trans
  where
    trans (PromptEvent.Character "esc") | onlyInsert flags =
      PromptUpdate PromptState.Quit CursorUpdate.Unmodified TextUpdate.Unmodified PromptConsumed.Yes
    trans (PromptEvent.Character "esc") =
      normal
    trans (PromptEvent.Character "c-n") =
      normal
    trans (PromptEvent.Character "bs") =
      insert CursorUpdate.OneLeft TextUpdate.DeleteLeft PromptConsumed.Yes
    trans (PromptEvent.Character c) | unprocessable c =
      insert CursorUpdate.Unmodified TextUpdate.Unmodified PromptConsumed.No
    trans (PromptEvent.Character "space") =
      insert CursorUpdate.OneRight (TextUpdate.Insert " ") PromptConsumed.Yes
    trans (PromptEvent.Character c) =
      insert CursorUpdate.OneRight (TextUpdate.Insert c) PromptConsumed.Yes
    trans _ =
      insert CursorUpdate.Unmodified TextUpdate.Unmodified PromptConsumed.No
    insert =
      PromptUpdate PromptState.Insert
    normal =
      PromptUpdate PromptState.Normal CursorUpdate.OneLeft TextUpdate.Unmodified PromptConsumed.Yes

basicTransition ::
  Monad m =>
  [PromptFlag] ->
  PromptEvent ->
  PromptState ->
  m PromptUpdate
basicTransition _ (PromptEvent.Set (Prompt cursor state text _)) _ =
  pure $ PromptUpdate state (CursorUpdate.Index cursor) (TextUpdate.Set text) PromptConsumed.Yes
basicTransition _ event PromptState.Normal =
  pure $ basicTransitionNormal event
basicTransition flags event PromptState.Insert =
  pure $ basicTransitionInsert flags event
basicTransition _ _ PromptState.Quit =
  pure $ PromptUpdate PromptState.Quit CursorUpdate.Unmodified TextUpdate.Unmodified PromptConsumed.No

pristinePrompt :: Bool -> Prompt
pristinePrompt insert =
  Prompt 0 (if insert then PromptState.Insert else PromptState.Normal) "" PromptRandom

noPromptRenderer ::
  Applicative m =>
  PromptRenderer m
noPromptRenderer =
  PromptRenderer unit (const unit) (const unit)
