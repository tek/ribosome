module Ribosome.Menu.Interpreter.PromptEvents where

import Control.Lens.Regex.Text (group, regex)
import qualified Data.Text as Text (isPrefixOf)
import Prelude hiding (group)

import Ribosome.Menu.Effect.PromptEvents (PromptEvents (HandlePromptEvent))
import Ribosome.Menu.Prompt.Data.CursorUpdate (CursorUpdate)
import qualified Ribosome.Menu.Prompt.Data.CursorUpdate as CursorUpdate (CursorUpdate (..))
import qualified Ribosome.Menu.Prompt.Data.PromptMode as PromptMode
import Ribosome.Menu.Prompt.Data.PromptMode (PromptMode)
import qualified Ribosome.Menu.Prompt.Data.PromptUpdate as PromptUpdate
import Ribosome.Menu.Prompt.Data.PromptUpdate (PromptUpdate)
import qualified Ribosome.Menu.Prompt.Data.TextUpdate as TextUpdate (TextUpdate (..))

stripTermcode :: Text -> Text
stripTermcode t =
  fromMaybe t (t ^? [regex|<(.*)>|] . group 0)

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
  Text ->
  PromptUpdate
basicTransitionNormal = \case
  "esc" ->
    PromptUpdate.Quit
  "q" ->
    PromptUpdate.Quit
  "i" ->
    consumeUnmodified PromptMode.Insert CursorUpdate.Unmodified
  "I" ->
    consumeUnmodified PromptMode.Insert CursorUpdate.Prepend
  "a" ->
    consumeUnmodified PromptMode.Insert CursorUpdate.OneRight
  "A" ->
    consumeUnmodified PromptMode.Insert CursorUpdate.Append
  "h" ->
    consumeUnmodified PromptMode.Normal CursorUpdate.OneLeft
  "l" ->
    consumeUnmodified PromptMode.Normal CursorUpdate.OneRight
  "x" ->
    PromptUpdate.Modify PromptMode.Normal CursorUpdate.OneLeft TextUpdate.DeleteRight
  _ ->
    PromptUpdate.Ignore

basicTransitionInsert ::
  Text ->
  PromptUpdate
basicTransitionInsert = \case
  "esc" ->
    normal
  "c-n" ->
    normal
  "bs" ->
    insert CursorUpdate.OneLeft TextUpdate.DeleteLeft
  c | unprocessable c ->
    PromptUpdate.Ignore
  "space" ->
    insert CursorUpdate.OneRight (TextUpdate.Insert " ")
  c ->
    insert CursorUpdate.OneRight (TextUpdate.Insert c)
  where
    insert =
      PromptUpdate.Modify PromptMode.Insert
    normal =
      PromptUpdate.Modify PromptMode.Normal CursorUpdate.OneLeft TextUpdate.Unmodified

interpretPromptEventsDefault :: InterpreterFor PromptEvents r
interpretPromptEventsDefault =
  interpret \case
    HandlePromptEvent key PromptMode.Insert -> do
      pure (basicTransitionInsert (stripTermcode key))
    HandlePromptEvent key PromptMode.Normal ->
      pure (basicTransitionNormal (stripTermcode key))
