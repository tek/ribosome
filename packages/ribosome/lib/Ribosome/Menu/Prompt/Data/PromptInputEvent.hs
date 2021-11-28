module Ribosome.Menu.Prompt.Data.PromptInputEvent where

data PromptInputEvent =
  Init
  |
  Character Text
  |
  Interrupt
  |
  Error Text
  deriving stock (Eq, Show)
