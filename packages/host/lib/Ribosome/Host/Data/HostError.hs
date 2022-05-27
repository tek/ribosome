module Ribosome.Host.Data.HostError where

import Ribosome.Host.Data.HandlerError (HandlerError)

data HostError =
  HostError {
    notification :: Bool,
    error :: HandlerError
  }
  deriving stock (Show)
