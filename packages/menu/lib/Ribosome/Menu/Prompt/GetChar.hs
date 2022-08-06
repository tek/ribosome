module Ribosome.Menu.Prompt.GetChar where

import qualified Data.Text as Text
import Exon (exon)
import qualified Log

import Ribosome.Menu.Effect.PromptEvents (PromptEvents, handlePromptEvent)
import Ribosome.Menu.Prompt.Data.CursorUpdate (CursorUpdate)
import qualified Ribosome.Menu.Prompt.Data.CursorUpdate as CursorUpdate (CursorUpdate (..))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt), PromptText (PromptText))
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import Ribosome.Menu.Prompt.Data.PromptMode (PromptMode)
import qualified Ribosome.Menu.Prompt.Data.PromptUpdate as PromptUpdate
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

modifyPrompt :: PromptMode -> CursorUpdate -> TextUpdate -> Prompt -> Prompt
modifyPrompt newState cursorUpdate textUpdate (Prompt cursor _ (PromptText text)) =
  Prompt updatedCursor newState (PromptText updatedText)
  where
    updatedCursor =
      updateCursor cursor updatedText cursorUpdate
    updatedText =
      updateText cursor text textUpdate

updatePrompt ::
  Member PromptEvents r =>
  Text ->
  Prompt ->
  Sem r PromptEvent
updatePrompt key prompt = do
  handlePromptEvent key (prompt ^. #state) <&> \case
    PromptUpdate.Modify newState cursorUpdate textUpdate ->
      PromptEvent.Update (modifyPrompt newState cursorUpdate textUpdate prompt)
    PromptUpdate.Ignore ->
      PromptEvent.Mapping key
    PromptUpdate.Quit ->
      PromptEvent.Quit Nothing

processChar ::
  Members [PromptEvents, Log] r =>
  Text ->
  Prompt ->
  Sem r PromptEvent
processChar key old = do
  action <- updatePrompt key old
  action <$ Log.debug [exon|prompt input: #{key} -> #{show action}|]
