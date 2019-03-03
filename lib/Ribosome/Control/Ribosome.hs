{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Ribosome.Control.Ribosome(
  Ribosome(..),
  RibosomeInternal(..),
  Locks,
  newRibosome,
  locks,
  errors,
  internal,
  public,
  name,
  state,
  RibosomeState(..),
  newRibosomeTVar,
) where

import Control.Lens (makeClassy)
import Control.Monad.IO.Class (MonadIO)
import Data.Default (def)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty)
import UnliftIO.STM (TMVar, TVar, newTVarIO)

import Ribosome.Data.Errors (Errors)

type Locks = Map String (TMVar ())

data RibosomeInternal =
  RibosomeInternal {
    _locks :: Locks,
    _errors :: Errors
  }

makeClassy ''RibosomeInternal

data RibosomeState s =
  RibosomeState {
    _internal :: RibosomeInternal,
    _public :: s
  }

makeClassy ''RibosomeState

data Ribosome s =
  Ribosome {
    _name :: String,
    _state :: TVar (RibosomeState s)
  }
makeClassy ''Ribosome

newRibosomeTVar :: MonadIO m => s -> m (TVar (RibosomeState s))
newRibosomeTVar s =
  newTVarIO (RibosomeState (RibosomeInternal Map.empty def) s)

newRibosome :: MonadIO m => String -> s -> m (Ribosome s)
newRibosome name' s = do
  tv <- newRibosomeTVar s
  return $ Ribosome name' tv
