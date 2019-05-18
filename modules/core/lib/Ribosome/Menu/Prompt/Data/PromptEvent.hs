module Ribosome.Menu.Prompt.Data.PromptEvent where

data PromptEvent =
  Init
  |
  Character Text
  |
  Unexpected Int
  deriving (Eq, Show)
