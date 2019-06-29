module Ribosome.Menu.Data.MenuAction where

import qualified Text.Show as Show (show)

import Ribosome.Menu.Data.MenuEvent (QuitReason)

data MenuAction m a =
  Quit (QuitReason m a)
  |
  Continue
  |
  Execute (m ())
  |
  Render Bool

instance Show (MenuAction m a) where
  show (Quit r) =
    "Quit(" <> show r <> ")"
  show Continue =
    "Continue"
  show (Execute _) =
    "Execute"
  show (Render changed) =
    "Render(" <> show changed <> ")"
