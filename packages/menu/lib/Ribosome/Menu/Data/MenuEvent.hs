module Ribosome.Menu.Data.MenuEvent where

import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data QueryEvent =
  Refined
  |
  Reset
  |
  Modal
  deriving stock (Eq, Show, Ord)

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
  deriving stock (Eq, Show, Ord)
