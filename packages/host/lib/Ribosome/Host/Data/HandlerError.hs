module Ribosome.Host.Data.HandlerError where

import Exon (exon)
import Polysemy.Log (Severity (Error))
import Prelude hiding (tag)
import Text.Show (showParen, showsPrec)
import GHC.Stack (withFrozenCallStack)

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

data HandlerError :: Type where
  HandlerError :: HasCallStack => {
    msg :: ErrorMessage,
    tag :: Maybe HandlerTag
  } -> HandlerError

instance Show HandlerError where
  showsPrec d HandlerError {..} =
    showParen (d > 10) [exon|HandlerError { msg = #{showsPrec 0 msg}, tag = #{showsPrec 0 tag} }|]

simple ::
  HasCallStack =>
  Text ->
  HandlerError
simple msg =
  withFrozenCallStack do
    HandlerError (ErrorMessage msg [msg] Error) Nothing

handlerError ::
  Member (Stop HandlerError) r =>
  HasCallStack =>
  ErrorMessage ->
  Maybe HandlerTag ->
  Sem r a
handlerError msg tag =
  withFrozenCallStack do
    stop (HandlerError msg tag)

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

mapHandlerError ::
  ToErrorMessage e =>
  Member (Stop HandlerError) r =>
  Sem (Stop e : r) a ->
  Sem r a
mapHandlerError =
  mapStop (toHandlerError Nothing)
