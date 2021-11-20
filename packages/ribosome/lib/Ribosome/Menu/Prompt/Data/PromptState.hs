module Ribosome.Menu.Prompt.Data.PromptState where

data PromptState =
  Insert
  |
  Normal
  |
  Quit
  deriving stock (Eq, Show)

deepLenses ''PromptState
