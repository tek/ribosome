module Ribosome.Menu.Interpreter.MenuState where

import Conc (Lock, interpretScopedWithH, lock)

import Ribosome.Host.Effect.MState (MState, mread, muse)
import Ribosome.Host.Interpreter.MState (interpretMState)
import Ribosome.Menu.Data.CursorIndex (CursorIndex)
import Ribosome.Menu.Data.MenuItems (MenuItems)
import Ribosome.Menu.Effect.MenuState (MenuState (..), ScopedMenuState)

type MenuStateDeps i =
  [
    MState (MenuItems i),
    MState CursorIndex
  ]

interpretMenuStateDeps ::
  ∀ i mres r .
  Members [Resource, Race, Mask mres, Embed IO] r =>
  InterpretersFor (MenuStateDeps i) r
interpretMenuStateDeps =
  interpretMState def .
  interpretMState def

itemsLock ::
  Members [Lock @@ "items", Resource] r =>
  Sem r a ->
  Sem r a
itemsLock =
  tag . lock . raise

cursorLock ::
  Members [Lock @@ "cursor", Resource] r =>
  Sem r a ->
  Sem r a
cursorLock =
  tag . lock . raise

mcState' ::
  Functor f =>
  (s -> m (s, a)) ->
  s ->
  Sem (WithTactics e f m r) (s, f a)
mcState' f s = do
  res <- runTSimple (f s)
  Inspector ins <- getInspectorT
  let newS = fromMaybe s (fst <$> ins res)
  pure (newS, snd <$> res)

mcState ::
  Functor f =>
  Member (MState s) r =>
  (s -> m (s, a)) ->
  Sem (WithTactics e f m r) (f a)
mcState f =
  muse (mcState' f)

handleMenuState ::
  Members (MenuStateDeps i) r =>
  MenuState i m x ->
  Tactical (MenuState i) m r x
handleMenuState = \case
  UseCursor f ->
    mcState f
  ReadCursor ->
    pureT =<< mread
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
