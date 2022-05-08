module Ribosome.Host.Effect.RequestHandler where

import Ribosome.Host.Data.Request (SomeRequest)
import Ribosome.Host.Data.Response (Response)

data RequestHandler :: Effect where
  Handle :: SomeRequest -> RequestHandler m Response

makeSem ''RequestHandler
