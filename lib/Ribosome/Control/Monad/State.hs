module Ribosome.Control.Monad.State(
  riboStateLocal,
) where

import Control.Lens (Lens', view, set)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.Trans.Class (MonadTrans(lift))
import qualified Ribosome.Control.Ribo as Ribo (inspect, modify)
import Ribosome.Control.Monad.RiboE (Ribo)

riboStateLocal ::
  (Monad (t (Ribo s)), MonadTrans t) =>
  Lens' s s' ->
  StateT s' (ExceptT e (t (Ribo s))) a ->
  ExceptT e (t (Ribo s)) a
riboStateLocal zoom ma = do
  state <- lift $ lift $ Ribo.inspect $ view zoom
  (output, newState) <- runStateT ma state
  lift $ lift $ Ribo.modify $ set zoom newState
  return output
