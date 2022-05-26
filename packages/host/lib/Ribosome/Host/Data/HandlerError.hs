module Ribosome.Host.Data.HandlerError where

import Polysemy.Log (Severity (Error))

newtype HandlerTag =
  HandlerTag { unHandlerTag :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

data ErrorMessage =
  ErrorMessage {
    user :: Text,
    log :: [Text],
    severity :: Severity
  }
  deriving stock (Eq, Show)

data HandlerError =
  HandlerError {
    msg :: ErrorMessage,
    tag :: Maybe HandlerTag
  }
  deriving stock (Eq, Show)

simple :: Text -> HandlerError
simple msg =
  HandlerError (ErrorMessage msg [msg] Error) Nothing

instance IsString HandlerError where
  fromString =
    simple . toText

class ToErrorMessage e where
  toErrorMessage :: e -> ErrorMessage

toHandlerError ::
  ToErrorMessage e =>
  Maybe HandlerTag ->
  e ->
  HandlerError
toHandlerError htag e =
  HandlerError (toErrorMessage e) htag

handlerErrorFrom ::
  ToErrorMessage e =>
  Member (Stop HandlerError) r =>
  HandlerTag ->
  Sem (Stop e : r) a ->
  Sem r a
handlerErrorFrom t =
  mapStop (toHandlerError (Just t))

handlerError ::
  ToErrorMessage e =>
  Member (Stop HandlerError) r =>
  Sem (Stop e : r) a ->
  Sem r a
handlerError =
  mapStop (toHandlerError Nothing)
