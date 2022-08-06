module Ribosome.Menu.Interpreter.MenuUiPure where

import Conc (Gate, interpretAtomic, interpretGate, interpretPScopedResumable_, subscribeWhile, withAsync_)
import Polysemy.Chronos (ChronosTime)
import Polysemy.Conc.Gate (gate, signal)
import qualified Time
import Time (MilliSeconds (MilliSeconds))

import Ribosome.Menu.Data.MenuEvent (MenuEvent (Exhausted))
import Ribosome.Menu.Effect.MenuUi (
  MenuUi (PromptEvent, Render, RenderPrompt),
  NvimMenuUi,
  PureMenu (PureMenu),
  )
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)

waitForItems ::
  Member (EventConsumer res MenuEvent) r =>
  Sem r ()
waitForItems =
  subscribeWhile (pure . (Exhausted /=))

initialWait ::
  Members [EventConsumer res MenuEvent, Gate] r =>
  Bool ->
  Sem r ()
initialWait waitExhausted = do
  when waitExhausted waitForItems
  signal

takeEvent ::
  Members [ChronosTime, AtomicState [PromptEvent]] r =>
  Sem r PromptEvent
takeEvent =
  maybe (PromptEvent.Ignore <$ Time.sleep (MilliSeconds 100)) pure =<< atomicState' \case
    [] -> ([], Nothing)
    (h : t) -> (t, Just h)

interpretMenuUiPureAtomic ::
  Members [Gate, AtomicState [PromptEvent], ChronosTime] r =>
  InterpreterFor (NvimMenuUi PureMenu) r
interpretMenuUiPureAtomic =
  interpretPScopedResumable_ (const (pure PureMenu)) \ PureMenu -> \case
    RenderPrompt _ ->
      unit
    PromptEvent _ -> do
      gate
      takeEvent
    Render _ ->
      unit

interpretMenuUiPure ::
  Members [EventConsumer res MenuEvent, ChronosTime, Resource, Async, Race, Embed IO] r =>
  Bool ->
  [PromptEvent] ->
  InterpreterFor (NvimMenuUi PureMenu) r
interpretMenuUiPure waitExhausted events =
  interpretGate .
  withAsync_ (initialWait waitExhausted) .
  interpretAtomic events .
  interpretMenuUiPureAtomic .
  raiseUnder2
