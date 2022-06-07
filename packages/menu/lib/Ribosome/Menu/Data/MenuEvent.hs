module Ribosome.Menu.Data.MenuEvent where

import Ribosome.Menu.Data.QuitReason (QuitReason)

data MenuEvent =
  Init
  |
  PromptEdit
  |
  PromptNavigation
  |
  Mapping Text
  |
  NewItems
  |
  Exhausted
  |
  Quit QuitReason
  deriving stock (Eq, Show)
