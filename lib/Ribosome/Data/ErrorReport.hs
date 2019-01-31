{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Data.ErrorReport(
  ErrorReport(..),
  user,
  log,
  priority,
) where

import Prelude hiding (log)
import Control.Lens (makeClassy)
import System.Log (Priority)

data ErrorReport =
  ErrorReport {
    _user :: String,
    _log :: [String],
    _priority :: Priority
  }
  deriving (Eq, Show)

makeClassy ''ErrorReport
