module Ribosome.Host.Effect.RequestHandler where

import Data.MessagePack (Object)

import Ribosome.Host.Data.Request (Request)

data RequestHandler :: Effect where
  Handle :: Request Object -> RequestHandler m Object

makeSem ''RequestHandler
