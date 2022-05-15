module Ribosome.Control.Ribosome where

import Data.MessagePack (Object)
import Path (Abs, Dir, Path)
import Prelude hiding (state)

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
  deriving stock (Generic)
  deriving anyclass (Default)

makeClassy ''RibosomeInternal

data RibosomeState s =
  RibosomeState {
    _internal :: RibosomeInternal,
    _public :: s
  }
  deriving stock (Generic)
  deriving anyclass (Default)

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
