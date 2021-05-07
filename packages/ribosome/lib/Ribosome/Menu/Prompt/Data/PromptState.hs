module Ribosome.Menu.Prompt.Data.PromptState where

data PromptState =
  Insert
  |
  Normal
  |
  Quit
  deriving (Eq, Show)

deepLenses ''PromptState
