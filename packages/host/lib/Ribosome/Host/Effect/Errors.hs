module Ribosome.Host.Effect.Errors where

import Ribosome.Host.Data.HandlerError (ErrorMessage, HandlerTag)
import Ribosome.Host.Data.StoredError (StoredError)

data Errors :: Effect where
  Store :: HandlerTag -> ErrorMessage -> Errors m ()
  Get :: Errors m (Map HandlerTag [StoredError])

makeSem ''Errors
