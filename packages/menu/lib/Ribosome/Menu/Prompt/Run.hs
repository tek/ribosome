module Ribosome.Menu.Prompt.Run where

import Conc (withAsync_)
import Data.Generics.Labels ()
import qualified Polysemy.Time as Time
import Prelude hiding (input, modifyMVar, output)
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Prelude (SerialT)
import Time (MilliSeconds (MilliSeconds), convert)

import Ribosome.Api.Input (syntheticInput)
import Ribosome.Final (inFinal_)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Menu.Effect.PromptControl (PromptControl, controlEvent, quitPrompt, startPrompt, waitPromptListening)
import Ribosome.Menu.Effect.PromptEvents (PromptEvents)
import qualified Ribosome.Menu.Effect.PromptInput as PromptInput
import Ribosome.Menu.Effect.PromptInput (PromptInput)
import qualified Ribosome.Menu.Effect.PromptRenderer as PromptRenderer
import Ribosome.Menu.Effect.PromptRenderer (PromptRenderer)
import qualified Ribosome.Menu.Effect.PromptState as PromptState
import Ribosome.Menu.Effect.PromptState (PromptState)
import qualified Ribosome.Menu.Prompt.Data.InputEvent as InputEvent
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt))
import Ribosome.Menu.Prompt.Data.PromptControlEvent (PromptControlEvent)
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import Ribosome.Menu.Prompt.Data.PromptInputEvent (PromptInputEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptInputEvent as PromptInputEvent (PromptInputEvent (..))
import qualified Ribosome.Menu.Prompt.Data.PromptMode as PromptMode (PromptMode (..))
import Ribosome.Menu.Stream.Util (takeUntilNothing)

inputEvent ::
  Members [PromptInput, Time t d] r =>
  (∀ x . Sem r x -> IO (Maybe x)) ->
  SerialT IO PromptInputEvent
inputEvent lower =
  spin
  where
    spin =
      Stream.fromEffect (lower PromptInput.event) >>= \case
        Just event -> case event of
          InputEvent.Character a ->
            Stream.cons (PromptInputEvent.Character a) spin
          InputEvent.Interrupt ->
            Stream.fromPure PromptInputEvent.Interrupt
          InputEvent.Error e ->
            Stream.fromPure (PromptInputEvent.Error e)
          InputEvent.NoInput ->
            Stream.before (void (lower (Time.sleep (MilliSeconds 33)))) spin
          InputEvent.Unexpected _ ->
            spin
        Nothing ->
          Stream.nil

type PromptStack =
  [PromptControl, PromptEvents, PromptInput, PromptRenderer, PromptState]

promptWithControl ::
  Members PromptStack r =>
  Members [Mask res, Time t d, Log, Resource] r =>
  (∀ x . Sem r x -> IO (Maybe x)) ->
  SerialT IO PromptControlEvent ->
  SerialT IO (Prompt, PromptEvent)
promptWithControl lower control =
  Stream.tapAsync (Fold.drainBy (lower . PromptRenderer.renderPrompt . fst)) $
  takeUntilNothing $
  Stream.mapM (fmap join . lower . PromptState.event) $
  Stream.parallelMin (Left <$> Stream.before (lower startPrompt) control) (Right <$> sourceWithInit)
  where
    sourceWithInit =
      Stream.cons PromptInputEvent.Init (inputEvent lower)

pristinePrompt :: Bool -> Prompt
pristinePrompt insert =
  Prompt 0 (if insert then PromptMode.Insert else PromptMode.Normal) ""

withPromptStream ::
  Members PromptStack r =>
  Members [Mask res, Time t d, Log, Resource, Race, Embed IO, Final IO] r =>
  (SerialT IO (Prompt, PromptEvent) -> Sem r a) ->
  Sem r a
withPromptStream use = do
  res <- inFinal_ \ lower lower_ pur -> do
    let
      backchannel =
        takeUntilNothing (Stream.repeatM (join <$> lower controlEvent))
    pur (Stream.finally (lower_ quitPrompt) (promptWithControl lower backchannel))
  use res

withPromptInput ::
  Members [PromptControl, Rpc, Resource, Race, Async, Time t d] r =>
  Maybe MilliSeconds ->
  [Text] ->
  Sem r a ->
  Sem r a
withPromptInput interval chrs =
  withAsync_ (waitPromptListening *> syntheticInput (convert <$> interval) chrs)
