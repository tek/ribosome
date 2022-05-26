module Ribosome.Menu.Prompt.Data.PromptConfig where

import Control.Lens (makeClassy, view)
import Streamly.Prelude (SerialT)

import Ribosome.Menu.Prompt.Data.PromptInputEvent (PromptInputEvent)
import Ribosome.Menu.Prompt.Data.PromptRenderer (PromptRenderer, hoistPromptRenderer)
import Ribosome.Menu.Prompt.Data.PromptState (PromptState)
import Ribosome.Menu.Prompt.Data.PromptUpdate (PromptUpdate)

data PromptFlag =
  StartInsert
  |
  OnlyInsert
  deriving stock (Eq, Show)

newtype PromptInput m =
  PromptInput { unPromptInput :: MVar () -> SerialT m PromptInputEvent }

newtype PromptEventHandler m =
  PromptEventHandler { unPromptEventHandler :: PromptInputEvent -> PromptState -> m PromptUpdate }

data PromptConfig m r =
  PromptConfig {
    _source :: PromptInput m,
    _handleEvent :: [PromptFlag] -> PromptEventHandler m,
    _render :: PromptRenderer r,
    _flags :: [PromptFlag]
  }

makeClassy ''PromptConfig

hoistPromptConfig ::
  (∀ x . Sem r x -> Sem r' x) ->
  PromptConfig m r ->
  PromptConfig m r'
hoistPromptConfig f PromptConfig {..} =
  PromptConfig {
    _render = hoistPromptRenderer f _render,
    ..
  }

class TestPromptFlag a where
  promptFlag :: PromptFlag -> a -> Bool

instance TestPromptFlag [PromptFlag] where
  promptFlag =
    elem

instance TestPromptFlag (PromptConfig m r) where
  promptFlag flag =
    promptFlag flag . view flags

startInsert :: TestPromptFlag a => a -> Bool
startInsert =
  promptFlag StartInsert

onlyInsert :: TestPromptFlag a => a -> Bool
onlyInsert =
  promptFlag OnlyInsert
