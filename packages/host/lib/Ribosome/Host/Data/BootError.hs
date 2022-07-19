-- |The fatal error type
module Ribosome.Host.Data.BootError where

-- |This type represents the singular fatal error used by Ribosome.
--
-- Contrary to all other errors, this one is used with 'Error' instead of 'Stop'.
-- It is only thrown from intialization code of interpreters when operation of the plugin is impossible due to the error
-- condition.
newtype BootError =
  BootError { unBootError :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)
