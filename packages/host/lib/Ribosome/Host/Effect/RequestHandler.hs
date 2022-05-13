module Ribosome.Host.Effect.RequestHandler where

import Ribosome.Host.Data.Request (SomeRequest)
import Ribosome.Host.Data.Response (Response)

data RequestHandler :: Effect where
  Request :: SomeRequest -> RequestHandler m Response
  Notification :: SomeRequest -> RequestHandler m ()

makeSem ''RequestHandler
