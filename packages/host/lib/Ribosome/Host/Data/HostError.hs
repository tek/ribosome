module Ribosome.Host.Data.HostError where

import Ribosome.Host.Data.HandlerError (HandlerError)

data HostError =
  HostError {
    async :: Bool,
    error :: HandlerError
  }
  deriving stock (Show)
