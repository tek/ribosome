module Ribosome.Host.Effect.RequestHandler where

import Data.MessagePack (Object)

import Ribosome.Host.Data.Request (Request)
import Ribosome.Host.Data.Response (Response)

data RequestHandler :: Effect where
  Handle :: Request Object -> RequestHandler m Response

makeSem ''RequestHandler
