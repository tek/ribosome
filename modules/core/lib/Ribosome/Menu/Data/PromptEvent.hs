module Ribosome.Menu.Data.PromptEvent where

data PromptEvent =
  Character Text
  |
  EOF
  deriving (Eq, Show)
