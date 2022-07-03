module Ribosome.Menu.Prompt.Data.PromptFlag where

data PromptFlag =
  StartInsert
  |
  OnlyInsert
  deriving stock (Eq, Show)

startInsert :: [PromptFlag] -> Bool
startInsert =
  elem StartInsert

onlyInsert :: [PromptFlag] -> Bool
onlyInsert =
  elem OnlyInsert
