{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Data.PersistError where

import Control.Lens (makeClassyPrisms)

data PersistError =
  FileNotReadable FilePath
  |
  NoSuchFile FilePath
  |
  Decode FilePath Text
  deriving (Eq, Show)

makeClassyPrisms ''PersistError
