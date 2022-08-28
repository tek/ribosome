-- |Disambiguation type for @Reader@.
module Ribosome.Data.CustomConfig where

-- |Disambiguation type used for the custom CLI configuration that is polymorphic in the stack.
newtype CustomConfig c =
  CustomConfig { unCustomConfig :: c }
  deriving stock (Eq, Show)
