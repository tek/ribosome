module Ribosome.Menu.Data.MenuAction where

import Exon (exon)

import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

-- | What to attempt to keep fixed when rendering updated state.
data RenderAnchor =
  -- | Attempt to keep the displayed cursor line the same, shift the displayed range of items so that the cursor index
  -- moves to the cursor line.
  -- This should be used for scrolling pagewise and entry changes.
  AnchorLine
  |
  -- | Attempt to keep the cursor index in the same displayed line, moving the cursor line to the index line.
  -- This should be used for cycling up and down by single lines.
  -- When the cursor line starts at the bottom or top, the index must be moved anyway.
  AnchorIndex
  deriving stock (Eq, Show, Generic)

data MenuAction a =
  Continue
  |
  Render RenderAnchor
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
  Render a -> [exon|Render #{show a}|]
  UpdatePrompt p -> [exon|UpdatePrompt #{show p}|]
  Quit res -> [exon|Quit #{MenuResult.describe res}|]
