module Ribosome.Menu.Data.MenuEvent where

import qualified Text.Show

import Ribosome.Menu.Data.MenuItem (MenuItem)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data QuitReason m a =
  Aborted
  |
  PromptError Text
  |
  NoOutput
  |
  Return a
  |
  Execute (m a)

instance Show (QuitReason m a) where
  show Aborted =
    "Aborted"
  show (PromptError err) =
    "PromptError(" <> toString err <> ")"
  show NoOutput =
    "NoOutput"
  show (Return _) =
    "Return"
  show (Execute _) =
    "Execute"

data MenuEvent m a i =
  Init Prompt
  |
  PromptChange Text Prompt
  |
  Mapping Text Prompt
  |
  NewItems (MenuItem i)
  |
  Quit (QuitReason m a)
  deriving Show
