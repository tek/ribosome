module Ribosome.Menu.Prompt.Run where

import Conduit (ConduitT, awaitForever, evalStateC, yield, (.|))
import Control.Monad.DeepState (modifyM')
import qualified Data.Text as Text (length, splitAt)

import Ribosome.Menu.Prompt.Data.CursorUpdate (CursorUpdate)
import qualified Ribosome.Menu.Prompt.Data.CursorUpdate as CursorUpdate (CursorUpdate(..))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt(Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig))
import Ribosome.Menu.Prompt.Data.PromptConsumerUpdate (PromptConsumerUpdate(PromptConsumerUpdate))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent(..))
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
      where
        (pre, post) = Text.splitAt cursor text

updatePrompt ::
  Monad m =>
  (PromptEvent -> PromptState -> m PromptUpdate) ->
  PromptEvent ->
  Prompt ->
  m Prompt
updatePrompt modes update (Prompt cursor state text) = do
  (PromptUpdate newState cursorUpdate textUpdate) <- modes update state
  return $ Prompt (updateCursor cursor text cursorUpdate) newState (updateText cursor text textUpdate)

processPromptEvent ::
  Monad m =>
  PromptConfig m ->
  PromptEvent ->
  ConduitT PromptEvent PromptConsumerUpdate (StateT Prompt m) ()
processPromptEvent (PromptConfig _ modes renderer) event = do
  newPrompt <- lift . modifyM' $ lift . updatePrompt modes event
  yield (PromptConsumerUpdate event newPrompt)
  lift . lift . renderer $ newPrompt

promptC ::
  Monad m =>
  PromptConfig m ->
  ConduitT () PromptConsumerUpdate m ()
promptC config@(PromptConfig source _ _) =
  source .| evalStateC pristinePrompt (awaitForever (processPromptEvent config))

basicTransition ::
  Monad m =>
  PromptEvent ->
  PromptState ->
  m PromptUpdate
basicTransition (PromptEvent.Character "esc") PromptState.Insert =
  return (PromptUpdate PromptState.Normal CursorUpdate.OneLeft TextUpdate.Unmodified)
basicTransition (PromptEvent.Character "i") PromptState.Normal =
  return (PromptUpdate PromptState.Insert CursorUpdate.Unmodified TextUpdate.Unmodified)
basicTransition (PromptEvent.Character "I") PromptState.Normal =
  return (PromptUpdate PromptState.Insert CursorUpdate.Prepend TextUpdate.Unmodified)
basicTransition (PromptEvent.Character "a") PromptState.Normal =
  return (PromptUpdate PromptState.Insert CursorUpdate.OneRight TextUpdate.Unmodified)
basicTransition (PromptEvent.Character "A") PromptState.Normal =
  return (PromptUpdate PromptState.Insert CursorUpdate.Append TextUpdate.Unmodified)
basicTransition (PromptEvent.Character c) PromptState.Insert =
  return (PromptUpdate PromptState.Insert CursorUpdate.OneRight (TextUpdate.Insert c))
basicTransition _ a =
  return (PromptUpdate a CursorUpdate.Unmodified TextUpdate.Unmodified)

pristinePrompt :: Prompt
pristinePrompt =
  Prompt 0 PromptState.Insert ""
