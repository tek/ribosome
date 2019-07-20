module Ribosome.Menu.Data.MenuAction where

import qualified Text.Show as Show (show)

import Ribosome.Menu.Data.MenuEvent (QuitReason)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data MenuAction m a =
  Quit (QuitReason m a)
  |
  Continue
  |
  Execute (m ())
  |
  Render Bool
  |
  UpdatePrompt Prompt

instance Show (MenuAction m a) where
  show (Quit r) =
    "Quit(" <> show r <> ")"
  show Continue =
    "Continue"
  show (Execute _) =
    "Execute"
  show (Render changed) =
    "Render(" <> show changed <> ")"
  show (UpdatePrompt prompt) =
    "UpdatePrompt(" <> show prompt <> ")"
