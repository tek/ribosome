module Ribosome.Menu.Interpreter.PromptEvents where

import qualified Data.Text as Text (isPrefixOf)

import Ribosome.Menu.Effect.PromptEvents (PromptEvents (HandlePromptEvent))
import Ribosome.Menu.Prompt.Data.CursorUpdate (CursorUpdate)
import qualified Ribosome.Menu.Prompt.Data.CursorUpdate as CursorUpdate (CursorUpdate (..))
import Ribosome.Menu.Prompt.Data.PromptFlag (PromptFlag, onlyInsert)
import Ribosome.Menu.Prompt.Data.PromptInputEvent (PromptInputEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptInputEvent as PromptInputEvent (PromptInputEvent (..))
import qualified Ribosome.Menu.Prompt.Data.PromptMode as PromptMode
import Ribosome.Menu.Prompt.Data.PromptMode (PromptMode)
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

consumeUnmodified :: PromptMode -> CursorUpdate -> PromptUpdate
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
    consumeUnmodified PromptMode.Insert CursorUpdate.Unmodified
  PromptInputEvent.Character "I" ->
    consumeUnmodified PromptMode.Insert CursorUpdate.Prepend
  PromptInputEvent.Character "a" ->
    consumeUnmodified PromptMode.Insert CursorUpdate.OneRight
  PromptInputEvent.Character "A" ->
    consumeUnmodified PromptMode.Insert CursorUpdate.Append
  PromptInputEvent.Character "h" ->
    consumeUnmodified PromptMode.Normal CursorUpdate.OneLeft
  PromptInputEvent.Character "l" ->
    consumeUnmodified PromptMode.Normal CursorUpdate.OneRight
  PromptInputEvent.Character "x" ->
    PromptUpdate.Modify PromptMode.Normal CursorUpdate.OneLeft TextUpdate.DeleteRight
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
      PromptUpdate.Modify PromptMode.Insert
    normal =
      PromptUpdate.Modify PromptMode.Normal CursorUpdate.OneLeft TextUpdate.Unmodified

interpretPromptEventsDefault ::
  [PromptFlag] ->
  InterpreterFor PromptEvents r
interpretPromptEventsDefault flags =
  interpret \case
    HandlePromptEvent event PromptMode.Insert ->
      pure (basicTransitionInsert flags event)
    HandlePromptEvent event PromptMode.Normal ->
      pure (basicTransitionNormal event)
