module Ribosome.Data.CustomConfig where

newtype CustomConfig c =
  CustomConfig { unCustomConfig :: c }
  deriving stock (Eq, Show)
