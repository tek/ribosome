module Ribosome.Host.Data.BootError where

newtype BootError =
  BootError { unBootError :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)
