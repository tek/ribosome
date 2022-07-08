module Ribosome.Menu.Data.MenuState where

import qualified Control.Monad.State as State
import Control.Monad.State (MonadState)
import Data.Generics.Labels ()

import Ribosome.Menu.Data.CursorIndex (CursorIndex)
import Ribosome.Menu.Data.Menu (Menu (Menu))
import Ribosome.Menu.Data.MenuAction (MenuAction)
import Ribosome.Menu.Data.MenuData (MenuItems)
import Ribosome.Menu.Effect.MenuState (MenuState, readItems, readPrompt, useCursor, useItems)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)

type ModifyMenu i r a =
  Sem (State (Menu i) : Reader Prompt : r) a

type ModifyCursor i r a =
  Sem (State CursorIndex : Reader (MenuItems i) : Reader Prompt : r) a

type MenuWidget r a =
  Sem r (Maybe (MenuAction a))

newtype SemS s r a =
  SemS { unSemS :: Sem (State s : r) a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadState s (SemS s r) where
  get = SemS get
  put = SemS . put

semState ::
  Member (State s) r =>
  SemS s r a ->
  Sem r a
semState =
  subsume . unSemS

liftCursor :: ModifyCursor i r a -> ModifyMenu i r a
liftCursor ma = do
  Menu i c <- get
  (newC, a) <- raise (runReader i (runState c ma))
  a <$ modify' (#cursor .~ newC)

runModifyMenu ::
  Member (MenuState i) r =>
  (Prompt -> ModifyMenu i r a) ->
  Sem r a
runModifyMenu f =
  useCursor \ cursor ->
    useItems \ items -> do
      prompt <- readPrompt
      (Menu newItems newCursor, result) <- runReader prompt (runState (Menu items cursor) (f prompt))
      pure (newItems, (newCursor, result))

runModifyCursor ::
  Member (MenuState i) r =>
  (Prompt -> ModifyCursor i r a) ->
  Sem r a
runModifyCursor action =
  useCursor \ cursor -> do
    items <- readItems
    prompt <- readPrompt
    runReader prompt (runReader items (runState cursor (action prompt)))

modifyMenu ::
  Member (MenuState i) r =>
  ModifyMenu i r a ->
  Sem r a
modifyMenu action =
  runModifyMenu (const action)

modifyCursor ::
  Member (MenuState i) r =>
  ModifyCursor i r a ->
  Sem r a
modifyCursor action =
  runModifyCursor (const action)
