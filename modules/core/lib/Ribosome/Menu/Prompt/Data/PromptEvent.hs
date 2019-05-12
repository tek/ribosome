module Ribosome.Menu.Prompt.Data.PromptEvent where

data PromptEvent =
  Init
  |
  Character Text
  |
  EOF
  deriving (Eq, Show)
