{-# LANGUAGE DeriveAnyClass #-}

module Ribosome.Control.Ribosome where

import Control.Concurrent.STM.TMVar (TMVar, TMVar, newTMVarIO)
import Control.Lens (makeClassy)
import Data.Map.Strict (Map)
import Data.MessagePack (Object)
import Prelude hiding (state)

import Path (Abs, Dir, Path)
import Ribosome.Data.Errors (Errors)
import Ribosome.Data.Scratch (Scratch)

type Locks = Map Text (TMVar ())

data RibosomeInternal =
  RibosomeInternal {
    _locks :: Locks,
    _errors :: Errors,
    _scratch :: Map Text Scratch,
    _watchedVariables :: Map Text Object,
    _projectDir :: Maybe (Path Abs Dir)
  }
  deriving (Generic, Default)

makeClassy ''RibosomeInternal

data RibosomeState s =
  RibosomeState {
    _internal :: RibosomeInternal,
    _public :: s
  }
  deriving (Generic, Default)

makeClassy ''RibosomeState

data Ribosome s =
  Ribosome {
    _name :: Text,
    _state :: TMVar (RibosomeState s)
  }

makeClassy ''Ribosome

newRibosomeTMVar :: MonadIO m => s -> m (TMVar (RibosomeState s))
newRibosomeTMVar s =
  liftIO $ newTMVarIO (RibosomeState def s)

newRibosome :: MonadIO m => Text -> s -> m (Ribosome s)
newRibosome name' =
  Ribosome name' <$$> newRibosomeTMVar
