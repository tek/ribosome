module Ribosome.Menu.Data.MenuState where

import Conc (Lock, lock)
import Control.Lens ((^.))
import qualified Control.Monad.State as State
import Control.Monad.State (MonadState)
import Data.Generics.Labels ()

import Ribosome.Menu.Data.Menu (Menu (Menu))
import Ribosome.Menu.Data.MenuAction (MenuAction)
import Ribosome.Menu.Data.MenuData (MenuCursor, MenuItems)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

data ItemsLock =
  ItemsLock
  deriving stock (Eq, Show)

itemsLock ::
  Members [Tagged ItemsLock Lock, Resource] r =>
  Sem r a ->
  Sem r a
itemsLock =
  tag . lock . raise

data CursorLock =
  CursorLock
  deriving stock (Eq, Show)

cursorLock ::
  Members [Tagged CursorLock Lock, Resource] r =>
  Sem r a ->
  Sem r a
cursorLock =
  tag . lock . raise

type MenuStateEffects i =
  [
    AtomicState (MenuItems i),
    AtomicState MenuCursor,
    AtomicState Prompt,
    Resource
  ]

type MenuStack i =
  MenuStateEffects i ++ [
    Tagged ItemsLock Lock,
    Tagged CursorLock Lock
  ]

type MenuWrite i r =
  Members (MenuStack i) r

type MenuRead i r =
  Members (Tagged CursorLock Lock : MenuStateEffects i) r

type WithMenu es i r a =
  Sem (es ++ MenuStack i ++ r) a

type MenuStateSem i r a =
  WithMenu '[] i r a

type MenuSem i r a =
  Sem (State (Menu i) : r) a

type MenuItemsSem r i a =
  WithMenu '[State (MenuItems i)] i r a

type MenuWidget r a =
  Sem r (Maybe (MenuAction a))

subsumeMenuStateSem ::
  Members (MenuStack i) r =>
  MenuStateSem i r a ->
  Sem r a
subsumeMenuStateSem =
  subsume .
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
  Sem (Reader (Menu i) : MenuStack i ++ r) (a, MenuCursor) ->
  MenuStateSem i r a
modifyMenuCursor f =
  cursorLock do
    menu <- readMenu
    (result, newCursor) <- runReader menu f
    atomicPut newCursor
    pure result

modifyMenuItems ::
  WithMenu '[Reader (Menu i)] i r (MenuItems i, a) ->
  MenuStateSem i r a
modifyMenuItems f =
  itemsLock do
    menu <- readMenu
    (newItems, result) <- runReader menu f
    atomicPut newItems
    pure result

menuItemsState ::
  (Prompt -> MenuCursor -> MenuItemsSem r i a) ->
  MenuStateSem i r a
menuItemsState f =
  modifyMenuItems do
    Menu i s p <- ask
    raise (runState i (f p s))

runMenu ::
  MenuWrite i r =>
  (Prompt -> Sem (State (Menu i) : r) a) ->
  Sem r a
runMenu f =
  itemsLock do
    cursorLock do
      menu <- readMenu
      (Menu newItems newState _, result) <- insertAt @0 (runState menu (f (menu ^. #prompt)))
      atomicPut newItems
      atomicPut newState
      pure result

runMenuRead ::
  MenuRead i r =>
  (Prompt -> MenuSem i r a) ->
  Sem r a
runMenuRead action =
  cursorLock do
    menu <- readMenu
    (newMenu, a) <- insertAt @0 (runState menu (action (menu ^. #prompt)))
    a <$ atomicPut (newMenu ^. #cursor)

setPrompt ::
  Member (AtomicState Prompt) r =>
  Prompt ->
  Sem r ()
setPrompt =
  atomicPut

menuWrite ::
  MenuWrite i r =>
  MenuSem i r a ->
  Sem r a
menuWrite action =
  runMenu (const action)

menuRead ::
  MenuRead i r =>
  MenuSem i r a ->
  Sem r a
menuRead action =
  insertAt @0 (runMenuRead (const action))
