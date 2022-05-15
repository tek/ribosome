module Ribosome.Data.Mapping where

newtype MappingIdent =
  MappingIdent Text
  deriving stock (Eq, Show)

data Mapping =
  Mapping {
    mappingIdent :: MappingIdent,
    mappingLhs :: Text,
    mappingMode :: Text,
    mappingRemap :: Bool,
    mappingBuffer :: Bool
  }
  deriving stock (Eq, Show)
