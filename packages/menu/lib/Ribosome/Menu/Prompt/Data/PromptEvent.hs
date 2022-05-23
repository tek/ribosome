module Ribosome.Menu.Prompt.Data.PromptEvent where

data PromptEvent =
  Init
  |
  Edit
  |
  Navigation
  |
  Mapping Text
  |
  Quit
  |
  Error Text
  deriving stock (Eq, Show)
