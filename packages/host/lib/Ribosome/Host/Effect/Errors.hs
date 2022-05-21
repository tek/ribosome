module Ribosome.Host.Effect.Errors where

import Ribosome.Host.Data.HandlerError (ErrorMessage, HandlerTag)

data Errors :: Effect where
  Store :: Maybe HandlerTag -> ErrorMessage -> Errors m ()
  Get :: Errors m (Map HandlerTag [ErrorMessage])

makeSem ''Errors
