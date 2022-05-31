module Ribosome.Menu.Prompt.Data.PromptConfig where

import Control.Lens (makeClassy, view)
import Streamly.Prelude (SerialT)

import Ribosome.Menu.Prompt.Data.PromptInputEvent (PromptInputEvent)
import Ribosome.Menu.Prompt.Data.PromptRenderer (PromptRenderer, hoistPromptRenderer)
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
    _source :: PromptInput,
    _handleEvent :: [PromptFlag] -> PromptEventHandler r,
    _render :: PromptRenderer r,
    _flags :: [PromptFlag]
  }

makeClassy ''PromptConfig

hoistPromptConfig ::
  (∀ x . Sem r x -> Sem r' x) ->
  PromptConfig r ->
  PromptConfig r'
hoistPromptConfig f PromptConfig {..} =
  PromptConfig {
    _handleEvent = \ flg -> PromptEventHandler \ e s -> f (unPromptEventHandler (_handleEvent flg) e s),
    _render = hoistPromptRenderer f _render,
    ..
  }

class TestPromptFlag a where
  promptFlag :: PromptFlag -> a -> Bool

instance TestPromptFlag [PromptFlag] where
  promptFlag =
    elem

instance TestPromptFlag (PromptConfig r) where
  promptFlag flag =
    promptFlag flag . view flags

startInsert :: TestPromptFlag a => a -> Bool
startInsert =
  promptFlag StartInsert

onlyInsert :: TestPromptFlag a => a -> Bool
onlyInsert =
  promptFlag OnlyInsert
