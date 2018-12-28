module Ribosome.Data.Errors(
  ComponentName(..),
  Error(..),
  Errors(..),
) where

import qualified Data.Map as Map
import Data.Default.Class (Default(def))
import Data.Map.Strict (Map)

newtype ComponentName =
  ComponentName String
  deriving (Eq, Ord, Show)

data Error =
  Error {
    errorTimestamp :: Int,
    errorMessage :: [String]
  }
  deriving Show

newtype Errors =
  Errors (Map ComponentName [Error])
  deriving Show

instance Default Errors where
  def = Errors Map.empty
