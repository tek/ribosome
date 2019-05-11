module Ribosome.Menu.Data.InputEvent where

data InputEvent =
  Character Text
  |
  NoInput
  |
  EOF
  deriving (Eq, Show)
