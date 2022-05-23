module Ribosome.Menu.Data.MenuAction where

import Exon (exon)
import Text.Show (showParen, showString, showsPrec)

import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data MenuAction r a =
  Continue
  |
  Render
  |
  UpdatePrompt Prompt
  |
  Quit (Sem r (MenuResult a))
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
  Sem r a ->
  MenuAction r a
success =
  Quit . fmap MenuResult.Success

abort ::
  MenuAction r a
abort =
  Quit (pure MenuResult.Aborted)

hoistMenuAction ::
  (∀ x . Sem r x -> Sem r' x) ->
  MenuAction r a ->
  MenuAction r' a
hoistMenuAction f = \case
  Continue -> Continue
  Render -> Render
  UpdatePrompt p -> UpdatePrompt p
  Quit ma -> Quit (f ma)
