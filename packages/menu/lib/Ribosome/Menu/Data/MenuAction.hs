module Ribosome.Menu.Data.MenuAction where

import Exon (exon)

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

describe ::
  MenuAction a ->
  Text
describe = \case
  Continue -> "Continue"
  Render -> "Render"
  UpdatePrompt p -> [exon|UpdatePrompt #{show p}|]
  Quit res -> [exon|Quit #{MenuResult.describe res}|]
