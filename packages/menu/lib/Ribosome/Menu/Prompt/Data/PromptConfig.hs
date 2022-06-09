module Ribosome.Menu.Prompt.Data.PromptConfig where

import Streamly.Prelude (SerialT)

import Ribosome.Menu.Prompt.Data.PromptInputEvent (PromptInputEvent)
import Ribosome.Menu.Prompt.Data.PromptState (PromptState)
import Ribosome.Menu.Prompt.Data.PromptUpdate (PromptUpdate)

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

newtype PromptEventHandler r =
  PromptEventHandler { unPromptEventHandler :: PromptInputEvent -> PromptState -> Sem r PromptUpdate }

data PromptConfig r =
  PromptConfig {
    source :: PromptInput,
    handleEvent :: [PromptFlag] -> PromptEventHandler r,
    flags :: [PromptFlag]
  }
  deriving stock (Generic)

hoistPromptConfig ::
  (∀ x . Sem r x -> Sem r' x) ->
  PromptConfig r ->
  PromptConfig r'
hoistPromptConfig f PromptConfig {..} =
  PromptConfig {
    handleEvent = \ flg -> PromptEventHandler \ e s -> f (unPromptEventHandler (handleEvent flg) e s),
    ..
  }

class TestPromptFlag a where
  promptFlag :: PromptFlag -> a -> Bool

instance TestPromptFlag [PromptFlag] where
  promptFlag =
    elem

instance TestPromptFlag (PromptConfig r) where
  promptFlag flag =
    promptFlag flag . flags

startInsert :: TestPromptFlag a => a -> Bool
startInsert =
  promptFlag StartInsert

onlyInsert :: TestPromptFlag a => a -> Bool
onlyInsert =
  promptFlag OnlyInsert
