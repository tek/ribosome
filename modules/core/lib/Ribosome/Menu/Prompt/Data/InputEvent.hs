module Ribosome.Menu.Prompt.Data.InputEvent where

data InputEvent =
  Character Text
  |
  NoInput
  |
  Unexpected Int
  deriving (Eq, Show)
