module Ribosome.Host.Data.HandlerError where

import Polysemy.Log (Severity (Error))

data HandlerError =
  HandlerError {
    user :: Text,
    log :: [Text],
    severity :: Severity
  }
  deriving stock (Eq, Show)

simple :: Text -> HandlerError
simple msg =
  HandlerError msg [msg] Error

instance IsString HandlerError where
  fromString =
    simple . toText

class ToHandlerError e where
  toHandlerError :: e -> HandlerError

handlerError ::
  ToHandlerError e =>
  Member (Error HandlerError) r =>
  Sem (Stop e : r) a ->
  Sem r a
handlerError =
  stopToError . mapStop toHandlerError . raiseUnder
