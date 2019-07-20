module Ribosome.Menu.Prompt.Data.PromptEvent where

data PromptEvent =
  Init
  |
  Character Text
  |
  -- SpecialCharacter Text
  -- |
  Unexpected Int
  |
  Interrupt
  |
  Error Text
  |
  Set Text
  deriving (Eq, Show)
