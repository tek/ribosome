module Ribosome.Menu.Prompt.Data.InputEvent where

data InputEvent =
  Character Text
  |
  NoInput
  |
  Interrupt
  deriving stock (Eq, Show)
