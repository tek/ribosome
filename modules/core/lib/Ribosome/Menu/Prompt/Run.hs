module Ribosome.Menu.Prompt.Run where

import Conduit (ConduitT, awaitForever, evalStateC, yield, (.|))
import Control.Monad.DeepState (modifyM')
import qualified Data.Text as Text (drop, dropEnd, length, splitAt)

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
  Monad m =>
  PromptConfig m ->
  PromptEvent ->
  ConduitT PromptEvent PromptConsumerUpdate (StateT Prompt m) ()
processPromptEvent (PromptConfig _ modes (PromptRenderer _ _ render) _) event = do
  consumed <- lift . stateM $ lift . updatePrompt modes event
  newPrompt <- get
  yield (PromptConsumerUpdate event newPrompt consumed)
  lift . lift . render $ newPrompt

promptC ::
  Monad m =>
  PromptConfig m ->
  ConduitT () PromptConsumerUpdate m ()
promptC config@(PromptConfig source _ _ insert) =
  (yield PromptEvent.Init *> source) .| evalStateC (pristinePrompt insert) (awaitForever (processPromptEvent config))

unprocessable :: [Text]
unprocessable =
  [
    "cr"
    ]

basicTransition ::
  Monad m =>
  PromptEvent ->
  PromptState ->
  m PromptUpdate
basicTransition (PromptEvent.Character "esc") PromptState.Insert =
  return (PromptUpdate PromptState.Normal CursorUpdate.OneLeft TextUpdate.Unmodified PromptConsumed.Yes)
basicTransition (PromptEvent.Character "bs") PromptState.Insert =
  return (PromptUpdate PromptState.Insert CursorUpdate.OneLeft TextUpdate.DeleteLeft PromptConsumed.Yes)
basicTransition (PromptEvent.Character "i") PromptState.Normal =
  return (PromptUpdate PromptState.Insert CursorUpdate.Unmodified TextUpdate.Unmodified PromptConsumed.Yes)
basicTransition (PromptEvent.Character "I") PromptState.Normal =
  return (PromptUpdate PromptState.Insert CursorUpdate.Prepend TextUpdate.Unmodified PromptConsumed.Yes)
basicTransition (PromptEvent.Character "a") PromptState.Normal =
  return (PromptUpdate PromptState.Insert CursorUpdate.OneRight TextUpdate.Unmodified PromptConsumed.Yes)
basicTransition (PromptEvent.Character "A") PromptState.Normal =
  return (PromptUpdate PromptState.Insert CursorUpdate.Append TextUpdate.Unmodified PromptConsumed.Yes)
basicTransition (PromptEvent.Character c) PromptState.Insert | c `elem` unprocessable =
  return (PromptUpdate PromptState.Insert CursorUpdate.Unmodified TextUpdate.Unmodified PromptConsumed.No)
basicTransition (PromptEvent.Character c) PromptState.Insert =
  return (PromptUpdate PromptState.Insert CursorUpdate.OneRight (TextUpdate.Insert c) PromptConsumed.Yes)
basicTransition (PromptEvent.Character "h") PromptState.Normal =
  return (PromptUpdate PromptState.Normal CursorUpdate.OneLeft TextUpdate.Unmodified PromptConsumed.Yes)
basicTransition (PromptEvent.Character "l") PromptState.Normal =
  return (PromptUpdate PromptState.Normal CursorUpdate.OneRight TextUpdate.Unmodified PromptConsumed.Yes)
basicTransition (PromptEvent.Character "x") PromptState.Normal =
  return (PromptUpdate PromptState.Normal CursorUpdate.OneLeft TextUpdate.DeleteRight PromptConsumed.Yes)
basicTransition _ a =
  return (PromptUpdate a CursorUpdate.Unmodified TextUpdate.Unmodified PromptConsumed.No)

pristinePrompt :: Bool -> Prompt
pristinePrompt insert =
  Prompt 0 (if insert then PromptState.Insert else PromptState.Normal) ""

noPromptRenderer ::
  Applicative m =>
  PromptRenderer m
noPromptRenderer =
  PromptRenderer unit (const unit) (const unit)
