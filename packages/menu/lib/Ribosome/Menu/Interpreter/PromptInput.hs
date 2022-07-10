module Ribosome.Menu.Interpreter.PromptInput where

import Conc (interpretAtomic, resultToMaybe)
import Polysemy.Chronos (ChronosTime)
import qualified Polysemy.Time as Time
import qualified Queue
import Time (MilliSeconds (MilliSeconds))

import Ribosome.Menu.Effect.NvimPromptInput (NvimPromptInput, getChar)
import Ribosome.Menu.Effect.PromptControl (PromptControl, waitPromptQuit)
import Ribosome.Menu.Effect.PromptInput (PromptInput (Event))
import qualified Ribosome.Menu.Prompt.Data.InputEvent as InputEvent
import Ribosome.Menu.Prompt.Data.InputEvent (InputEvent)

interpretPromptInputList ::
  Members [ChronosTime, Embed IO] r =>
  [InputEvent] ->
  InterpreterFor PromptInput r
interpretPromptInputList events =
  interpretAtomic events .
  reinterpret \case
    Event ->
      maybe (InputEvent.NoInput <$ Time.sleep (MilliSeconds 100)) pure =<< atomicState' \case
        [] -> ([], Nothing)
        (h : t) -> (t, Just h)

interpretPromptInputCharList ::
  Members [ChronosTime, Embed IO] r =>
  [Text] ->
  InterpreterFor PromptInput r
interpretPromptInputCharList cs =
  interpretPromptInputList (InputEvent.Character <$> cs)

nvimPromptInput ::
  Members [PromptControl, NvimPromptInput] r =>
  InterpreterFor PromptInput r
nvimPromptInput =
  interpret \case
    Event ->
      getChar waitPromptQuit

interpretPromptInputQueue ::
  Member (Queue InputEvent) r =>
  InterpreterFor PromptInput r
interpretPromptInputQueue =
  interpret \case
    Event ->
      fromMaybe InputEvent.Interrupt . resultToMaybe <$> Queue.read
