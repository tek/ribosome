{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Ribosome.Control.Ribosome(
  Ribosome (..),
  RibosomeInternal (..),
  Locks,
  newRibosome,
  _locks,
  _errors,
  newInternalTVar,
  _internal,
) where

import Control.Lens (makeClassy_)
import Control.Monad.IO.Class (MonadIO)
import Data.Default (def)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty)
import UnliftIO.STM (TVar, newTVarIO, TMVar)

import Ribosome.Data.Errors (Errors)

type Locks = Map String (TMVar ())

data RibosomeInternal =
  RibosomeInternal {
    locks :: Locks,
    errors :: Errors
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
newInternalTVar = newTVarIO (RibosomeInternal Map.empty def)

newRibosome :: MonadIO m => String -> e -> m (Ribosome (TVar e))
newRibosome name' env' = do
  envTv <- newTVarIO env'
  intTv <- newInternalTVar
  return $ Ribosome name' intTv envTv
