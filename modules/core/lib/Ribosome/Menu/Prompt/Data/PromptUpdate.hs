module Ribosome.Menu.Prompt.Data.PromptUpdate where

import Ribosome.Menu.Prompt.Data.CursorUpdate (CursorUpdate)
import Ribosome.Menu.Prompt.Data.PromptConsumed (PromptConsumed)
import Ribosome.Menu.Prompt.Data.PromptState (PromptState)
import Ribosome.Menu.Prompt.Data.TextUpdate (TextUpdate)

data PromptUpdate =
  PromptUpdate {
    _state :: PromptState,
    _cursor :: CursorUpdate,
    _text :: TextUpdate,
    _consumed :: PromptConsumed
  }
  deriving (Eq, Show)
