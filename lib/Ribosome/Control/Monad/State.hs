module Ribosome.Control.Monad.State(
  riboStateLocalT,
  riboStateT,
  riboStateLocal,
  riboState,
  runRiboState,
  modifyL,
  prepend,
) where

import Control.Lens (Lens')
import qualified Control.Lens as Lens (over, view, set)
import Control.Monad.State.Class (MonadState, modify)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State (StateT, runStateT)
import Ribosome.Control.Monad.RiboE (Ribo, RiboE(..), runRiboE)
import qualified Ribosome.Control.Ribo as Ribo (inspect, modify)

riboStateLocalT ::
  (Monad (t (Ribo s)), MonadTrans t) =>
  Lens' s s' ->
  StateT s' (ExceptT e (t (Ribo s))) a ->
  ExceptT e (t (Ribo s)) a
riboStateLocalT zoom ma = do
  state <- lift $ lift $ Ribo.inspect $ Lens.view zoom
  (output, newState) <- runStateT ma state
  lift $ lift $ Ribo.modify $ Lens.set zoom newState
  return output

riboStateT ::
  (Monad (t (Ribo s)), MonadTrans t) =>
  StateT s (ExceptT e (t (Ribo s))) a ->
  ExceptT e (t (Ribo s)) a
riboStateT =
  riboStateLocalT id

riboStateLocal ::
  Lens' s s' ->
  StateT s' (ExceptT e (Ribo s)) a ->
  RiboE s e a
riboStateLocal zoom ma = RiboE $ do
  state <- lift $ Ribo.inspect $ Lens.view zoom
  (output, newState) <- runStateT ma state
  lift $ Ribo.modify $ Lens.set zoom newState
  return output

riboState ::
  StateT s (ExceptT e (Ribo s)) a ->
  RiboE s e a
riboState =
  riboStateLocal id

runRiboState ::
  StateT s (ExceptT e (Ribo s)) a ->
  Ribo s (Either e a)
runRiboState = runRiboE . riboState

modifyL ::
  (MonadState s m) =>
  Lens' s a ->
  (a -> a) ->
  m ()
modifyL lens f =
  modify $ Lens.over lens f

prepend ::
  (MonadState s m) =>
  Lens' s [a] ->
  a ->
  m ()
prepend lens a =
  modifyL lens (a :)
