module Ribosome.Menu.Prompt.Data.PromptConfig where

data PromptConfig =
  StartNormal
  |
  StartInsert
  |
  OnlyInsert
  deriving stock (Eq, Show)

instance Default PromptConfig where
  def =
    StartNormal

isStartInsert :: PromptConfig -> Bool
isStartInsert = \case
  StartNormal -> False
  StartInsert -> True
  OnlyInsert -> True

startInsert :: PromptConfig
startInsert =
  StartInsert

onlyInsert :: PromptConfig
onlyInsert =
  OnlyInsert
