module Ribosome.Menu.Data.PromptUpdate where

import Ribosome.Menu.Data.CursorUpdate (CursorUpdate)
import Ribosome.Menu.Data.PromptState (PromptState)

data PromptUpdate =
  PromptUpdate {
    _state :: PromptState,
    _cursor :: CursorUpdate
  }
  deriving (Eq, Show)
