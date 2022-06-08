module Ribosome.Menu.Data.MenuAction where

import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data MenuAction a =
  Continue
  |
  Render
  |
  UpdatePrompt Prompt
  |
  Quit (MenuResult a)
  deriving stock (Eq, Show, Functor)

success ::
  a ->
  MenuAction a
success =
  Quit . MenuResult.Success

abort ::
  MenuAction a
abort =
  Quit MenuResult.Aborted
