module Ribosome.Menu.Prompt.Data.PromptEvent where

import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

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
  Set Prompt
  deriving stock (Eq, Show)
