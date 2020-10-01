{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Data.ErrorReport(
  ErrorReport(..),
  user,
  log,
  priority,
) where

import System.Log (Priority)

data ErrorReport =
  ErrorReport {
    _user :: Text,
    _log :: [Text],
    _priority :: Priority
  }
  deriving (Eq, Show)

makeClassy ''ErrorReport
