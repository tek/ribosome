module Ribosome.Menu.Data.MenuAction where

import Exon (exon)
import Text.Show (showParen, showString, showsPrec)

import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data MenuAction m a =
  Continue
  |
  Render
  |
  UpdatePrompt Prompt
  |
  Quit (m (MenuResult a))
  deriving stock (Functor)

instance Show (MenuAction m a) where
  showsPrec d = \case
    Continue ->
      showString "Continue"
    Render ->
      showString "Render"
    UpdatePrompt prompt ->
      showParen (d > 10) [exon|UpdatePrompt #{showsPrec 11 prompt}|]
    Quit _ ->
      showString "Quit"

success ::
  Applicative m =>
  m a ->
  MenuAction m a
success =
  Quit . fmap MenuResult.Success

abort ::
  Applicative m =>
  MenuAction m a
abort =
  Quit (pure MenuResult.Aborted)
