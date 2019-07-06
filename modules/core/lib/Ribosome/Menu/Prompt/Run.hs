module Ribosome.Menu.Prompt.Run where

import Conduit (ConduitT, await, awaitForever, evalStateC, yield, (.|))
import Data.Conduit.Combinators (peek)
import qualified Data.Text as Text (drop, dropEnd, length, splitAt)
import Prelude hiding (state)

import Ribosome.Control.Monad.Ribo (MonadRibo)
import Ribosome.Log (logDebug)
import Ribosome.Menu.Prompt.Data.CursorUpdate (CursorUpdate)
import qualified Ribosome.Menu.Prompt.Data.CursorUpdate as CursorUpdate (CursorUpdate(..))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt(Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig))
import Ribosome.Menu.Prompt.Data.PromptConsumed (PromptConsumed)
import qualified Ribosome.Menu.Prompt.Data.PromptConsumed as PromptConsumed (PromptConsumed(..))
import Ribosome.Menu.Prompt.Data.PromptConsumerUpdate (PromptConsumerUpdate(PromptConsumerUpdate))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent(..))
import Ribosome.Menu.Prompt.Data.PromptRenderer (PromptRenderer(PromptRenderer))
import Ribosome.Menu.Prompt.Data.PromptState (PromptState)
import qualified Ribosome.Menu.Prompt.Data.PromptState as PromptState (PromptState(..))
import Ribosome.Menu.Prompt.Data.PromptUpdate (PromptUpdate(PromptUpdate))
import Ribosome.Menu.Prompt.Data.TextUpdate (TextUpdate)
import qualified Ribosome.Menu.Prompt.Data.TextUpdate as TextUpdate (TextUpdate(..))

updateCursor :: Int -> Text -> CursorUpdate -> Int
updateCursor current text =
  update
  where
    update CursorUpdate.OneLeft | current > 0 =
      current - 1
    update CursorUpdate.OneRight | current <= Text.length text =
      current + 1
    update CursorUpdate.Prepend =
      0
    update CursorUpdate.Append =
      Text.length text + 1
    update _ =
      current

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
    (pre, post) = Text.splitAt cursor text

updatePrompt ::
  Monad m =>
  (PromptEvent -> PromptState -> m PromptUpdate) ->
  PromptEvent ->
  Prompt ->
  m (PromptConsumed, Prompt)
updatePrompt modes update (Prompt cursor state text) = do
  (PromptUpdate newState cursorUpdate textUpdate consumed) <- modes update state
  let
    newPrompt =
      Prompt (updateCursor cursor text cursorUpdate) newState (updateText cursor text textUpdate)
  return (consumed, newPrompt)

processPromptEvent ::
  MonadIO m =>
  MonadRibo m =>
  PromptConfig m ->
  PromptEvent ->
  ConduitT PromptEvent PromptConsumerUpdate (StateT Prompt m) ()
processPromptEvent (PromptConfig _ modes (PromptRenderer _ _ render) _) event = do
  logDebug @Text $ "prompt event: " <> show event
  consumed <- lift . stateM $ lift . updatePrompt modes event
  newPrompt <- get
  yield (PromptConsumerUpdate event newPrompt consumed)
  lift . lift . render $ newPrompt

skippingRenderer ::
  Monad m =>
  (Prompt -> m ()) ->
  ConduitT PromptConsumerUpdate PromptConsumerUpdate m ()
skippingRenderer render =
  go
  where
    go =
      check =<< await
    check (Just next@(PromptConsumerUpdate _ prompt _)) = do
      yield next
      renderIfIdle prompt =<< peek
      go
    check Nothing =
      return ()
    renderIfIdle _ (Just _) =
      return ()
    renderIfIdle prompt Nothing =
      lift (render prompt)

promptC ::
  MonadIO m =>
  MonadRibo m =>
  PromptConfig m ->
  ConduitT () PromptConsumerUpdate m ()
promptC config@(PromptConfig source _ (PromptRenderer _ _ render) insert) =
  sourceWithInit .| process .| skippingRenderer render
  where
    sourceWithInit =
      yield PromptEvent.Init *> source
    process =
      evalStateC (pristinePrompt insert) (awaitForever (processPromptEvent config))

unprocessable :: [Text]
unprocessable =
  [
    "cr"
    ]

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
  PromptEvent ->
  PromptUpdate
basicTransitionInsert (PromptEvent.Character "esc") =
  PromptUpdate PromptState.Normal CursorUpdate.OneLeft TextUpdate.Unmodified PromptConsumed.Yes
basicTransitionInsert (PromptEvent.Character "bs") =
  PromptUpdate PromptState.Insert CursorUpdate.OneLeft TextUpdate.DeleteLeft PromptConsumed.Yes
basicTransitionInsert (PromptEvent.Character c) | c `elem` unprocessable =
  PromptUpdate PromptState.Insert CursorUpdate.Unmodified TextUpdate.Unmodified PromptConsumed.No
basicTransitionInsert (PromptEvent.Character "space") =
  PromptUpdate PromptState.Insert CursorUpdate.OneRight (TextUpdate.Insert " ") PromptConsumed.Yes
basicTransitionInsert (PromptEvent.Character c) =
  PromptUpdate PromptState.Insert CursorUpdate.OneRight (TextUpdate.Insert c) PromptConsumed.Yes
basicTransitionInsert _ =
  PromptUpdate PromptState.Insert CursorUpdate.Unmodified TextUpdate.Unmodified PromptConsumed.No

basicTransition ::
  Monad m =>
  PromptEvent ->
  PromptState ->
  m PromptUpdate
basicTransition event PromptState.Normal =
  return $ basicTransitionNormal event
basicTransition event PromptState.Insert =
  return $ basicTransitionInsert event
basicTransition _ PromptState.Quit =
  return $ PromptUpdate PromptState.Quit CursorUpdate.Unmodified TextUpdate.Unmodified PromptConsumed.No

pristinePrompt :: Bool -> Prompt
pristinePrompt insert =
  Prompt 0 (if insert then PromptState.Insert else PromptState.Normal) ""

noPromptRenderer ::
  Applicative m =>
  PromptRenderer m
noPromptRenderer =
  PromptRenderer unit (const unit) (const unit)
