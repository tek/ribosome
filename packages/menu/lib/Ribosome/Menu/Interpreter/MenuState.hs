module Ribosome.Menu.Interpreter.MenuState where

import Conc (Lock, interpretAtomic, interpretSync, lock)

import Ribosome.Host.Effect.MState (MState, mread, muse)
import Ribosome.Host.Interpreter.MState (interpretMState)
import Ribosome.Menu.Data.CursorIndex (CursorIndex)
import Ribosome.Menu.Data.MenuConfig (MenuConfig)
import Ribosome.Menu.Data.MenuData (MenuItems)
import Ribosome.Menu.Effect.MenuFilter (MenuFilter)
import Ribosome.Menu.Effect.MenuState (MenuState (..))
import Ribosome.Menu.Effect.MenuStream (MenuStream)
import Ribosome.Menu.Effect.PromptControl (PromptControl)
import Ribosome.Menu.Effect.PromptEvents (PromptEvents)
import Ribosome.Menu.Effect.PromptState (PromptState)
import Ribosome.Menu.Interpreter.MenuFilter (interpretMenuFilterFuzzyWith)
import Ribosome.Menu.Interpreter.MenuStream (interpretMenuStream)
import Ribosome.Menu.Interpreter.PromptControl (interpretPromptControl)
import Ribosome.Menu.Interpreter.PromptEvents (interpretPromptEventsDefault)
import Ribosome.Menu.Interpreter.PromptState (interpretPromptState)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptFlag (PromptFlag)
import Ribosome.Menu.Prompt.Data.PromptListening (PromptListening)
import Ribosome.Menu.Prompt.Data.PromptQuit (PromptQuit)

type MenuStateDeps i =
  [
    Sync PromptQuit,
    Sync PromptListening,
    MState (MenuItems i),
    MState CursorIndex,
    AtomicState Prompt
  ]

interpretMenuStateDeps ::
  ∀ i mres r .
  Members [Resource, Race, Mask mres, Embed IO] r =>
  InterpretersFor (MenuStateDeps i) r
interpretMenuStateDeps =
  interpretAtomic def .
  interpretMState def .
  interpretMState def .
  interpretSync @PromptListening .
  interpretSync @PromptQuit

itemsLock ::
  Members [Tagged "items" Lock, Resource] r =>
  Sem r a ->
  Sem r a
itemsLock =
  tag . lock . raise

cursorLock ::
  Members [Tagged "cursor" Lock, Resource] r =>
  Sem r a ->
  Sem r a
cursorLock =
  tag . lock . raise

mcState ::
  Functor f =>
  Member (MState s) r =>
  (s -> Sem r0 (s, a)) ->
  Sem (WithTactics e f (Sem r0) r) (f a)
mcState f =
  muse \ s -> do
    res <- runTSimple (f s)
    Inspector ins <- getInspectorT
    let newS = fromMaybe s (fst <$> ins res)
    pure (newS, snd <$> res)

interpretMenuState ::
  ∀ i mres r .
  Members [Resource, Race, Mask mres, Embed IO] r =>
  InterpreterFor (MenuState i) r
interpretMenuState =
  interpretMenuStateDeps .
  interpretH \case
    UseCursor f ->
      mcState f
    ReadCursor ->
      pureT =<< mread
    SetPrompt p ->
      pureT =<< atomicPut p
    ReadPrompt ->
      pureT =<< atomicGet
    UseItems f ->
      mcState f
    ReadItems ->
      pureT =<< mread
  . insertAt @1

type MenuStack i =
  [MenuState i, MenuStream i, MenuFilter, PromptControl, PromptState, PromptEvents, Reader (MenuConfig i)]

interpretMenu ::
  ∀ i mres r .
  Members [Log, Resource, Race, Mask mres, Embed IO, Final IO] r =>
  MenuConfig i ->
  [PromptFlag] ->
  Bool ->
  InterpretersFor (MenuStack i) r
interpretMenu conf flags monotonic =
  runReader conf .
  interpretPromptEventsDefault flags .
  interpretPromptState flags .
  interpretPromptControl .
  interpretMenuFilterFuzzyWith monotonic .
  interpretMenuStream .
  interpretMenuState
