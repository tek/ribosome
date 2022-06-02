module Ribosome.Menu.Data.MenuState where

import Control.Lens ((^.))
import qualified Control.Monad.State as State
import Control.Monad.State (MonadState)
import qualified Polysemy.Conc as Sync

import Ribosome.Menu.Data.Menu (Menu (Menu), cursor, prompt)
import Ribosome.Menu.Data.MenuData (MenuCursor, MenuItems)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data ItemsLock =
  ItemsLock
  deriving stock (Eq, Show)

itemsLock ::
  Members [Sync ItemsLock, Resource] r =>
  Sem r a ->
  Sem r a
itemsLock =
  Sync.lock ItemsLock

data CursorLock =
  CursorLock
  deriving stock (Eq, Show)

cursorLock ::
  Members [Sync CursorLock, Resource] r =>
  Sem r a ->
  Sem r a
cursorLock =
  Sync.lock CursorLock

type MenuStateEffects i =
  [
    AtomicState (MenuItems i),
    AtomicState MenuCursor,
    AtomicState Prompt
  ]

type MenuStateStack i =
  [
    AtomicState (MenuItems i),
    AtomicState MenuCursor,
    AtomicState Prompt,
    Sync ItemsLock,
    Sync CursorLock
  ]

type WithMenu es i r a =
  Sem (es ++ MenuStateStack i ++ r) a

type MenuStateSem i r a =
  WithMenu '[] i r a

type MenuSem i r a =
  Sem (State (Menu i) : r) a

type MenuItemsSem r i a =
  WithMenu '[State (MenuItems i)] i r a

subsumeMenuStateSem ::
  Members (MenuStateStack i) r =>
  MenuStateSem i r a ->
  Sem r a
subsumeMenuStateSem =
  subsume .
  subsume .
  subsume .
  subsume .
  subsume

newtype SemS s r a =
  SemS { unSemS :: Sem (State s : r) a }
  deriving newtype (Functor, Applicative, Monad)

type MenuItemsSemS r i a =
  SemS (MenuItems i) r a

semState :: SemS s r a -> Sem (State s : r) a
semState =
  unSemS

instance MonadState s (SemS s r) where
  get = SemS get
  put = SemS . put

readMenu ::
  Members (MenuStateEffects i) r =>
  Sem r (Menu i)
readMenu =
  Menu <$> atomicGet <*> atomicGet <*> atomicGet

modifyMenuCursor ::
  Members [Resource, Embed IO] r =>
  Sem (Reader (Menu i) : MenuStateStack i ++ r) (a, MenuCursor) ->
  MenuStateSem i r a
modifyMenuCursor f =
  cursorLock do
    menu <- readMenu
    (result, newCursor) <- runReader menu f
    atomicPut newCursor
    pure result

modifyMenuItems ::
  Members [Resource, Embed IO] r =>
  WithMenu '[Reader (Menu i)] i r (MenuItems i, a) ->
  MenuStateSem i r a
modifyMenuItems f =
  itemsLock do
    menu <- readMenu
    (newItems, result) <- runReader menu f
    atomicPut newItems
    pure result

menuItemsState ::
  Members [Resource, Embed IO] r =>
  (Prompt -> MenuCursor -> MenuItemsSem r i a) ->
  MenuStateSem i r a
menuItemsState f =
  modifyMenuItems do
    Menu i s p <- ask
    raise (runState i (f p s))

runMenu ::
  Members [Resource, Embed IO] r =>
  (Prompt -> Sem (State (Menu i) : r) a) ->
  MenuStateSem i r a
runMenu f =
  itemsLock do
    cursorLock do
      menu <- readMenu
      (Menu newItems newState _, result) <- insertAt @0 (runState menu (f (menu ^. prompt)))
      atomicPut newItems
      atomicPut newState
      pure result

runMenuRead ::
  Members [Resource, Embed IO] r =>
  (Prompt -> MenuSem i r a) ->
  MenuStateSem i r a
runMenuRead action =
  cursorLock do
    menu <- readMenu
    (newMenu, a) <- insertAt @0 (runState menu (action (menu ^. prompt)))
    a <$ atomicPut (newMenu ^. cursor)

setPrompt ::
  Member (AtomicState Prompt) r =>
  Prompt ->
  Sem r ()
setPrompt =
  atomicPut

menuWrite ::
  Members [Resource, Embed IO] r =>
  MenuSem i r a ->
  MenuStateSem i r a
menuWrite action =
  insertAt @0 (runMenu (const action))

menuRead ::
  Members [Resource, Embed IO] r =>
  MenuSem i r a ->
  MenuStateSem i r a
menuRead action =
  insertAt @0 (runMenuRead (const action))
