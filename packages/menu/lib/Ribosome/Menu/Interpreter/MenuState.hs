module Ribosome.Menu.Interpreter.MenuState where

import Conc (Lock, lock)

import Ribosome.Host.Effect.MState (MState, mread, muse)
import Ribosome.Host.Interpreter.MState (interpretMState)
import Ribosome.Menu.Data.CursorIndex (CursorIndex)
import qualified Ribosome.Menu.Data.MenuItems as MenuItems
import Ribosome.Menu.Data.MenuItems (MenuItems)
import Ribosome.Menu.Effect.MenuState (MenuState (..))

type MenuStateDeps f i =
  [
    MState (MenuItems f i),
    MState CursorIndex
  ]

interpretMenuStateDeps ::
  ∀ f i mres r .
  Members [Resource, Race, Mask mres, Embed IO] r =>
  f ->
  InterpretersFor (MenuStateDeps f i) r
interpretMenuStateDeps initialFilter =
  interpretMState def .
  interpretMState (MenuItems.cons initialFilter)

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

mstateT ::
  Functor f =>
  (s -> m (s, a)) ->
  s ->
  Sem (WithTactics e f m r) (s, f a)
mstateT f s = do
  res <- runTSimple (f s)
  Inspector ins <- getInspectorT
  let newS = fromMaybe s (fst <$> ins res)
  pure (newS, snd <$> res)

handleMenuState ::
  Members (MenuStateDeps f i) r =>
  MenuState f i m x ->
  Tactical (MenuState f i) m r x
handleMenuState = \case
  UseCursor f ->
    muse (mstateT f)
  ReadCursor ->
    pureT =<< mread
  UseItems f ->
    muse (mstateT f)
  ReadItems ->
    pureT =<< mread

interpretMenuState ::
  ∀ f i mres r .
  Members [Resource, Race, Mask mres, Embed IO] r =>
  f ->
  InterpreterFor (MenuState f i) r
interpretMenuState initialFilter =
  interpretMenuStateDeps initialFilter .
  interpretH handleMenuState .
  insertAt @1

scope ::
  Members [Resource, Race, Mask mres, Embed IO] r =>
  f ->
  (() -> Sem (MenuStateDeps f i ++ r) a) ->
  Sem r a
scope initialFilter use =
  interpretMenuStateDeps initialFilter do
    use ()
