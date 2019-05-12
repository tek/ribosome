module Ribosome.Menu.Prompt.Data.PromptEvent where

data PromptEvent =
  Character Text
  |
  EOF
  deriving (Eq, Show)
