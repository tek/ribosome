module Ribosome.Menu.Interpreter.MenuState where

import Conc (Lock, interpretAtomic, interpretScopedWithH, interpretSync, lock)

import Ribosome.Host.Effect.MState (MState, ScopedMState, mread, muse)
import Ribosome.Host.Interpreter.MState (interpretMState, interpretMStates)
import Ribosome.Menu.Data.CursorIndex (CursorIndex)
import Ribosome.Menu.Data.MenuConfig (MenuConfig)
import Ribosome.Menu.Data.MenuData (MenuItems)
import Ribosome.Menu.Effect.MenuFilter (Fuzzy, MenuFilter)
import Ribosome.Menu.Effect.MenuState (MenuState (..), ScopedMenuState, withMenuState)
import Ribosome.Menu.Effect.MenuStream (MenuStream)
import Ribosome.Menu.Effect.PromptControl (PromptControl, withPromptControl)
import Ribosome.Menu.Effect.PromptEvents (PromptEvents)
import Ribosome.Menu.Effect.PromptState (PromptState)
import Ribosome.Menu.Effect.PromptStream (PromptStream)
import Ribosome.Menu.Interpreter.MenuFilter (BoolVal, interpretMenuFilterFuzzy)
import Ribosome.Menu.Interpreter.MenuStream (interpretMenuStream)
import Ribosome.Menu.Interpreter.PromptControl (interpretPromptControl)
import Ribosome.Menu.Interpreter.PromptEvents (interpretPromptEventsDefault)
import Ribosome.Menu.Interpreter.PromptState (interpretPromptState)
import Ribosome.Menu.Interpreter.PromptStream (interpretPromptStream)
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
  (s -> m (s, a)) ->
  Sem (WithTactics e f m r) (f a)
mcState f =
  muse \ s -> do
    res <- runTSimple (f s)
    Inspector ins <- getInspectorT
    let newS = fromMaybe s (fst <$> ins res)
    pure (newS, snd <$> res)

handleMenuState ::
  Members (MenuStateDeps i) r =>
  MenuState i m x ->
  Tactical (MenuState i) m r x
handleMenuState = \case
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

interpretMenuState ::
  ∀ i mres r .
  Members [Resource, Race, Mask mres, Embed IO] r =>
  InterpreterFor (MenuState i) r
interpretMenuState =
  interpretMenuStateDeps .
  interpretH handleMenuState .
  insertAt @1

scope ::
  Members [Resource, Race, Mask mres, Embed IO] r =>
  (() -> Sem (MenuStateDeps i ++ r) a) ->
  Sem r a
scope use =
  interpretMenuStateDeps do
    use ()

interpretMenuStates ::
  ∀ i mres r .
  Members [Resource, Race, Mask mres, Embed IO] r =>
  InterpreterFor (ScopedMenuState i) r
interpretMenuStates =
  interpretScopedWithH @(MenuStateDeps i) scope \ () e -> handleMenuState e

type MenuStack i =
  [MenuState i, MenuFilter, PromptControl, PromptState, PromptEvents, Reader (MenuConfig i)]

type MenuIOStack i =
  [MenuStream i, PromptStream] ++ MenuStack i ++ [
    Scoped () PromptControl,
    Scoped () (MenuState i),
    ScopedMState Prompt
  ]

runMenu ::
  ∀ mono i r .
  Member (MenuFilter @@ Fuzzy mono) r =>
  Members [Scoped () PromptControl, ScopedMState Prompt, Scoped () (MenuState i), Log] r =>
  MenuConfig i ->
  [PromptFlag] ->
  InterpretersFor (MenuStack i) r
runMenu conf flags =
  runReader conf .
  interpretPromptEventsDefault flags .
  interpretPromptState flags .
  withPromptControl .
  tag .
  withMenuState

interpretMenuFinal ::
  ∀ (mono :: Bool) i mres r .
  BoolVal mono =>
  Members [Log, Resource, Race, Mask mres, Embed IO, Final IO] r =>
  MenuConfig i ->
  [PromptFlag] ->
  InterpretersFor (MenuIOStack i) r
interpretMenuFinal conf flags =
  interpretMStates .
  interpretMenuStates .
  interpretPromptControl .
  interpretMenuFilterFuzzy @mono . untag .
  runMenu @mono conf flags .
  insertAt @6 @'[MenuFilter @@ Fuzzy mono] .
  interpretPromptStream .
  interpretMenuStream
