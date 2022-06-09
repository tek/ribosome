module Ribosome.Menu.Prompt.Data.PromptConfig where

import Streamly.Prelude (SerialT)

import Ribosome.Menu.Prompt.Data.PromptInputEvent (PromptInputEvent)

data PromptListening =
  PromptListening
  deriving stock (Eq, Show)

data PromptFlag =
  StartInsert
  |
  OnlyInsert
  deriving stock (Eq, Show)

newtype PromptInput =
  PromptInput { unPromptInput :: MVar () -> SerialT IO PromptInputEvent }

data PromptConfig =
  PromptConfig {
    source :: PromptInput,
    flags :: [PromptFlag]
  }
  deriving stock (Generic)

startInsert :: [PromptFlag] -> Bool
startInsert =
  elem StartInsert

onlyInsert :: [PromptFlag] -> Bool
onlyInsert =
  elem OnlyInsert
