module Ribosome.Menu.Prompt.Transition where

import qualified Data.Text as Text (isPrefixOf)

import Ribosome.Menu.Prompt.Data.CursorUpdate (CursorUpdate)
import qualified Ribosome.Menu.Prompt.Data.CursorUpdate as CursorUpdate (CursorUpdate (..))
import Ribosome.Menu.Prompt.Data.PromptConfig (
  PromptEventHandler (PromptEventHandler),
  PromptFlag,
  onlyInsert,
  )
import Ribosome.Menu.Prompt.Data.PromptInputEvent (PromptInputEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptInputEvent as PromptInputEvent (PromptInputEvent (..))
import Ribosome.Menu.Prompt.Data.PromptState (PromptState)
import qualified Ribosome.Menu.Prompt.Data.PromptState as PromptState (PromptState (..))
import qualified Ribosome.Menu.Prompt.Data.PromptUpdate as PromptUpdate
import Ribosome.Menu.Prompt.Data.PromptUpdate (PromptUpdate)
import qualified Ribosome.Menu.Prompt.Data.TextUpdate as TextUpdate (TextUpdate (..))

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
  PromptUpdate.Modify s u TextUpdate.Unmodified

basicTransitionNormal ::
  PromptInputEvent ->
  PromptUpdate
basicTransitionNormal = \case
  PromptInputEvent.Character "esc" ->
    PromptUpdate.Quit
  PromptInputEvent.Character "q" ->
    PromptUpdate.Quit
  PromptInputEvent.Character "i" ->
    consumeUnmodified PromptState.Insert CursorUpdate.Unmodified
  PromptInputEvent.Character "I" ->
    consumeUnmodified PromptState.Insert CursorUpdate.Prepend
  PromptInputEvent.Character "a" ->
    consumeUnmodified PromptState.Insert CursorUpdate.OneRight
  PromptInputEvent.Character "A" ->
    consumeUnmodified PromptState.Insert CursorUpdate.Append
  PromptInputEvent.Character "h" ->
    consumeUnmodified PromptState.Normal CursorUpdate.OneLeft
  PromptInputEvent.Character "l" ->
    consumeUnmodified PromptState.Normal CursorUpdate.OneRight
  PromptInputEvent.Character "x" ->
    PromptUpdate.Modify PromptState.Normal CursorUpdate.OneLeft TextUpdate.DeleteRight
  _ ->
    PromptUpdate.Ignore

basicTransitionInsert ::
  [PromptFlag] ->
  PromptInputEvent ->
  PromptUpdate
basicTransitionInsert flags = \case
  PromptInputEvent.Character "esc" | onlyInsert flags ->
    PromptUpdate.Quit
  PromptInputEvent.Character "esc" ->
    normal
  PromptInputEvent.Character "c-n" ->
    normal
  PromptInputEvent.Character "bs" ->
    insert CursorUpdate.OneLeft TextUpdate.DeleteLeft
  PromptInputEvent.Character c | unprocessable c ->
    PromptUpdate.Ignore
  PromptInputEvent.Character "space" ->
    insert CursorUpdate.OneRight (TextUpdate.Insert " ")
  PromptInputEvent.Character c ->
    insert CursorUpdate.OneRight (TextUpdate.Insert c)
  _ ->
    PromptUpdate.Ignore
  where
    insert =
      PromptUpdate.Modify PromptState.Insert
    normal =
      PromptUpdate.Modify PromptState.Normal CursorUpdate.OneLeft TextUpdate.Unmodified

basicTransition ::
  [PromptFlag] ->
  PromptEventHandler r
basicTransition flags =
  PromptEventHandler \case
    event -> pure . \case
      PromptState.Insert ->
        basicTransitionInsert flags event
      PromptState.Normal ->
        basicTransitionNormal event
