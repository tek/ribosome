module Ribosome.Host.Effect.RequestHandler where

import Ribosome.Host.Data.Request (Request)
import Ribosome.Host.Data.Response (Response)

data RequestHandler :: Effect where
  Request :: Request -> RequestHandler m Response
  Notification :: Request -> RequestHandler m ()

makeSem ''RequestHandler
