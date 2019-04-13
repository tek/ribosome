{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Control.Ribosome where

import Control.Lens (makeClassy)
import Control.Monad.IO.Class (MonadIO)
import Data.Default (def)
import Data.Functor.Syntax ((<$$>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty)
import UnliftIO.STM (TMVar, TVar, newTVarIO)

import Ribosome.Data.Errors (Errors)
import Ribosome.Data.Scratch (Scratch)

type Locks = Map String (TMVar ())

data RibosomeInternal =
  RibosomeInternal {
    _locks :: Locks,
    _errors :: Errors,
    _scratch :: Map String Scratch
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
  newTVarIO (RibosomeState (RibosomeInternal Map.empty def Map.empty) s)

newRibosome :: MonadIO m => String -> s -> m (Ribosome s)
newRibosome name' =
  Ribosome name' <$$> newRibosomeTVar
