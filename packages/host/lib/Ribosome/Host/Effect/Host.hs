module Ribosome.Host.Effect.Host where

import Ribosome.Host.Data.Request (Request)
import Ribosome.Host.Data.Response (Response)

data Host :: Effect where
  Request :: Request -> Host m Response
  Notification :: Request -> Host m ()

makeSem ''Host
