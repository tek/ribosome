module Ribosome.Menu.Data.MenuEvent where

import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data QueryEvent =
  Refined
  |
  Reset
  |
  History
  deriving stock (Eq, Show)

data MenuEvent =
  Query QueryEvent
  |
  Inserted
  |
  PromptUpdated Prompt
  |
  PromptLoop
  |
  Rendered
  |
  Exhausted
  deriving stock (Eq, Show)
