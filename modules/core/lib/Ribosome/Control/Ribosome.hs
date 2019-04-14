{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Ribosome.Control.Ribosome where

import Control.Lens (makeClassy)
import Control.Monad.IO.Class (MonadIO)
import Data.Default (Default(def))
import Data.Functor.Syntax ((<$$>))
import Data.Map (Map)
import Data.MessagePack (Object)
import GHC.Generics (Generic)
import UnliftIO.STM (TMVar, TVar)

import Ribosome.Data.Errors (Errors)
import Ribosome.Data.Scratch (Scratch)

type Locks = Map Text (TMVar ())

data RibosomeInternal =
  RibosomeInternal {
    _locks :: Locks,
    _errors :: Errors,
    _scratch :: Map Text Scratch,
    _watchedVariables :: Map Text Object
  }
  deriving (Generic, Default)

makeClassy ''RibosomeInternal

data RibosomeState s =
  RibosomeState {
    _internal :: RibosomeInternal,
    _public :: s
  }

makeClassy ''RibosomeState

data Ribosome s =
  Ribosome {
    _name :: Text,
    _state :: TVar (RibosomeState s)
  }
makeClassy ''Ribosome

newRibosomeTVar :: MonadIO m => s -> m (TVar (RibosomeState s))
newRibosomeTVar s =
  newTVarIO (RibosomeState def s)

newRibosome :: MonadIO m => Text -> s -> m (Ribosome s)
newRibosome name' =
  Ribosome name' <$$> newRibosomeTVar
