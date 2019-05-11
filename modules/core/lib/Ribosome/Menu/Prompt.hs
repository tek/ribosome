module Ribosome.Menu.Prompt where

import Conduit (ConduitT, awaitForever, evalStateC, yield, (.|))
import Control.Monad.DeepState (modifyM')
import qualified Data.Text as Text (length)

import Ribosome.Menu.Data.CursorUpdate (CursorUpdate)
import qualified Ribosome.Menu.Data.CursorUpdate as CursorUpdate (CursorUpdate(..))
import Ribosome.Menu.Data.Prompt (Prompt(Prompt))
import Ribosome.Menu.Data.PromptConfig (PromptConfig(PromptConfig))
import Ribosome.Menu.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Data.PromptEvent as PromptEvent (PromptEvent(..))
import Ribosome.Menu.Data.PromptState (PromptState)
import qualified Ribosome.Menu.Data.PromptState as PromptState (PromptState(..))
import Ribosome.Menu.Data.PromptUpdate (PromptUpdate(PromptUpdate))
import Ribosome.Menu.Prompt.Data.PromptConsumerUpdate (PromptConsumerUpdate(PromptConsumerUpdate))

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

updatePrompt ::
  Monad m =>
  (PromptEvent -> PromptState -> m PromptUpdate) ->
  PromptEvent ->
  Prompt ->
  m Prompt
updatePrompt modes update (Prompt cursor state text) = do
  (PromptUpdate newState cursorUpdate) <- modes update state
  return $ Prompt (updateCursor cursor text cursorUpdate) newState text

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
  return (PromptUpdate PromptState.Normal CursorUpdate.OneLeft)
basicTransition (PromptEvent.Character "i") PromptState.Normal =
  return (PromptUpdate PromptState.Insert CursorUpdate.Unmodified)
basicTransition (PromptEvent.Character "I") PromptState.Normal =
  return (PromptUpdate PromptState.Insert CursorUpdate.Prepend)
basicTransition (PromptEvent.Character "a") PromptState.Normal =
  return (PromptUpdate PromptState.Insert CursorUpdate.OneRight)
basicTransition (PromptEvent.Character "A") PromptState.Normal =
  return (PromptUpdate PromptState.Insert CursorUpdate.Append)
basicTransition _ a =
  return (PromptUpdate a CursorUpdate.Unmodified)

pristinePrompt :: Prompt
pristinePrompt =
  Prompt 0 PromptState.Insert ""
