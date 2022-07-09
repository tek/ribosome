module Ribosome.Menu.Prompt.Run where

import Conc (withAsync_)
import Data.Generics.Labels ()
import Prelude hiding (input, modifyMVar, output)
import Streamly.Prelude (SerialT)
import Time (MilliSeconds, convert)

import Ribosome.Api.Input (syntheticInput)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Menu.Effect.PromptControl (PromptControl, controlEvent, quitPrompt, startPrompt, waitPromptListening)
import Ribosome.Menu.Effect.PromptEvents (PromptEvents)
import qualified Ribosome.Menu.Effect.PromptInput as PromptInput
import Ribosome.Menu.Effect.PromptInput (PromptInput)
import Ribosome.Menu.Effect.PromptRenderer (PromptRenderer, renderPrompt)
import qualified Ribosome.Menu.Effect.PromptState as PromptState
import Ribosome.Menu.Effect.PromptState (PromptState)
import Ribosome.Menu.Effect.PromptStream (PromptStream, promptStream)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptMode as PromptMode (PromptMode (..))

type PromptStack =
  [PromptControl, PromptEvents, PromptInput, PromptRenderer, PromptState]

pristinePrompt :: Bool -> Prompt
pristinePrompt insert =
  Prompt 0 (if insert then PromptMode.Insert else PromptMode.Normal) ""

promptEventStream ::
  Members PromptStack r =>
  Member PromptStream r =>
  Sem r (SerialT IO (Prompt, PromptEvent))
promptEventStream =
  promptStream controlEvent renderPrompt PromptState.event startPrompt quitPrompt PromptInput.event

withPromptInput ::
  Members [PromptControl, Rpc, Resource, Race, Async, Time t d] r =>
  Maybe MilliSeconds ->
  [Text] ->
  Sem r a ->
  Sem r a
withPromptInput interval chrs =
  withAsync_ (waitPromptListening *> syntheticInput (convert <$> interval) chrs)
