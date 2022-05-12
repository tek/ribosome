module Ribosome.Host.Data.Args where

newtype Args =
  Args { unArgs :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

newtype JsonArgs a =
  JsonArgs { unJsonArgs :: a }
  deriving stock (Eq, Show)
