module Ribosome.Menu.Data.MenuAction where

import Exon (exon)

import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data MenuAction f a =
  Continue
  |
  Render
  |
  UpdatePrompt Prompt
  |
  ChangeFilter f
  |
  Quit (MenuResult a)
  deriving stock (Eq, Show, Functor)

success ::
  a ->
  MenuAction f a
success =
  Quit . MenuResult.Success

abort ::
  MenuAction f a
abort =
  Quit MenuResult.Aborted

describe ::
  Show f =>
  MenuAction f a ->
  Text
describe = \case
  Continue -> "Continue"
  Render -> "Render"
  UpdatePrompt p -> [exon|UpdatePrompt #{show p}|]
  ChangeFilter f -> [exon|ChangeFilter #{show f}|]
  Quit res -> [exon|Quit #{MenuResult.describe res}|]
