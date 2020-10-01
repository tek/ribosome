{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Data.Mapping where

import Data.MessagePack (Object)
import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Error.Report.Class (ReportError(..))
import System.Log (Priority(NOTICE, ERROR))

newtype MappingIdent =
  MappingIdent Text
  deriving (Eq, Show)

data Mapping =
  Mapping {
    mappingIdent :: MappingIdent,
    mappingLhs :: Text,
    mappingMode :: Text,
    mappingRemap :: Bool,
    mappingBuffer :: Bool
  }
  deriving (Eq, Show)

data MappingError =
  NoSuchMapping MappingIdent
  |
  InvalidArgs [Object]
  deriving (Eq, Show)

deepPrisms ''MappingError

instance ReportError MappingError where
  errorReport (NoSuchMapping (MappingIdent ident)) =
    ErrorReport ("no mapping defined for `" <> ident <>"`") ["no such mapping: " <> ident] NOTICE
  errorReport (InvalidArgs args) =
    ErrorReport "internal error while executing mapping" ["invalid mapping args:", show args] ERROR
