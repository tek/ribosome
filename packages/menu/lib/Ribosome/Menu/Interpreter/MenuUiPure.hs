module Ribosome.Menu.Interpreter.MenuUiPure where

import Conc (Gate, interpretAtomic, interpretGate, subscribeWhile, withAsync_)
import Polysemy.Chronos (ChronosTime)
import Polysemy.Conc.Gate (gate, signal)
import qualified Time
import Time (MilliSeconds (MilliSeconds))

import Ribosome.Menu.Data.MenuEvent (MenuEvent (Exhausted))
import Ribosome.Menu.Effect.MenuUi (
  MenuUi (ItemsScratch, PromptEvent, PromptScratch, Render, RenderPrompt, StatusScratch),
  PureMenu (PureMenu),
  ScopedMenuUi,
  )
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)

waitForItems ::
  Member (EventConsumer MenuEvent) r =>
  Sem r ()
waitForItems =
  subscribeWhile (pure . (Exhausted /=))

initialWait ::
  Members [EventConsumer MenuEvent, Gate] r =>
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
  InterpreterFor (ScopedMenuUi p) r
interpretMenuUiPureAtomic =
  interpretScopedR_ (const (pure PureMenu)) \ PureMenu -> \case
    RenderPrompt _ _ ->
      unit
    PromptEvent -> do
      gate
      takeEvent
    Render _ ->
      unit
    PromptScratch ->
      stop "No scratch with pure interpreter"
    StatusScratch ->
      stop "No scratch with pure interpreter"
    ItemsScratch ->
      stop "No scratch with pure interpreter"

interpretMenuUiPure ::
  Members [EventConsumer MenuEvent, ChronosTime, Resource, Async, Race, Embed IO] r =>
  Bool ->
  [PromptEvent] ->
  InterpreterFor (ScopedMenuUi p) r
interpretMenuUiPure waitExhausted events =
  interpretGate .
  withAsync_ (initialWait waitExhausted) .
  interpretAtomic events .
  interpretMenuUiPureAtomic .
  raiseUnder2
