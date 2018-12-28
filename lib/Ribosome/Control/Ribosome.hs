{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Ribosome.Control.Ribosome(
  Ribosome (..),
  newRibosome,
  RibosomeInternal (..),
  _internal,
  _locks,
  Locks,
  newInternalTVar,
) where

import Control.Lens (makeClassy_)
import UnliftIO.STM (TVar, newTVarIO, TMVar)
import Control.Monad.IO.Class (MonadIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty)

type Locks = Map String (TMVar ())

newtype RibosomeInternal =
  RibosomeInternal {
    locks :: Locks
  }
makeClassy_ ''RibosomeInternal

data Ribosome e =
  Ribosome {
    name :: String,
    internal :: TVar RibosomeInternal,
    env :: e
  }
makeClassy_ ''Ribosome

newInternalTVar :: MonadIO m => m (TVar RibosomeInternal)
newInternalTVar = newTVarIO (RibosomeInternal Map.empty)

newRibosome :: MonadIO m => String -> e -> m (Ribosome (TVar e))
newRibosome name' env' = do
  envTv <- newTVarIO env'
  intTv <- newInternalTVar
  return $ Ribosome name' intTv envTv
