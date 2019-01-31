{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Data.Errors(
  ComponentName(..),
  Errors(..),
  Error(..),
  componentErrors,
  timestamp,
  report,
) where

import Control.Lens (makeClassy)
import Data.Default (Default(def))
import Data.Map.Strict (Map)
import Prelude hiding (error)

import Ribosome.Data.ErrorReport (ErrorReport)

newtype ComponentName =
  ComponentName String
  deriving (Eq, Ord, Show)

data Error =
  Error {
    _timestamp :: Int,
    _report :: ErrorReport
  }
  deriving (Eq, Show)

makeClassy ''Error

newtype Errors =
  Errors {
    _componentErrors :: Map ComponentName [Error]
    }
  deriving (Eq, Show, Default)

makeClassy ''Errors
