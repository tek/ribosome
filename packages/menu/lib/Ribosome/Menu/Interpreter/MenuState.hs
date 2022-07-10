module Ribosome.Menu.Interpreter.MenuState where

import Conc (Lock, interpretAtomic, interpretScopedWithH, interpretSync, lock)

import Ribosome.Host.Effect.MState (MState, mread, muse)
import Ribosome.Host.Interpreter.MState (interpretMState)
import Ribosome.Menu.Data.CursorIndex (CursorIndex)
import Ribosome.Menu.Data.MenuData (MenuItems)
import Ribosome.Menu.Effect.MenuState (MenuState (..), ScopedMenuState)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
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
