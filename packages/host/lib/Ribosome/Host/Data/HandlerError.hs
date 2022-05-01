module Ribosome.Host.Data.HandlerError where

newtype HandlerError =
  HandlerError { unHandlerError :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString)
