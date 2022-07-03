module Ribosome.Menu.Prompt.Input where

import Control.Concurrent (threadDelay)
import qualified Streamly.Prelude as Stream
import Streamly.Prelude (SerialT)

import Ribosome.Menu.Prompt.Data.PromptConfig (PromptInput (PromptInput))
import qualified Ribosome.Menu.Prompt.Data.PromptInputEvent as PromptInputEvent

sleepIO ::
  Double ->
  IO ()
sleepIO t =
  threadDelay (round (t * 1000000))

promptInputWith ::
  Maybe Double ->
  Maybe Double ->
  SerialT IO Text ->
  PromptInput
promptInputWith delay interval chars =
  PromptInput do
    traverse_ (Stream.fromEffect . sleepIO) delay
    maybe id Stream.delay interval (PromptInputEvent.Character <$> chars)

promptInput ::
  [Text] ->
  PromptInput
promptInput =
  promptInputWith Nothing Nothing . Stream.fromList
