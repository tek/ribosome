module Ribosome.Host.Data.HandlerError where

import Exon (exon)
import GHC.Stack (withFrozenCallStack)
import Polysemy.Log (Severity (Error))
import Prelude hiding (tag)
import Text.Show (showParen, showsPrec)

data HandlerTag =
  GlobalTag
  |
  HandlerTag Text
  deriving stock (Eq, Show)

instance Ord HandlerTag where
  compare GlobalTag GlobalTag = EQ
  compare GlobalTag (HandlerTag _) = LT
  compare (HandlerTag _) GlobalTag = GT
  compare (HandlerTag l) (HandlerTag r) = compare l r

instance IsString HandlerTag where
  fromString =
    HandlerTag . toText

data ErrorMessage =
  ErrorMessage {
    user :: Text,
    log :: [Text],
    severity :: Severity
  }
  deriving stock (Eq, Show)

instance IsString ErrorMessage where
  fromString (toText -> s) =
    ErrorMessage s [s] Error

data HandlerError :: Type where
  HandlerError :: HasCallStack => {
    msg :: ErrorMessage,
    tag :: HandlerTag
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
    HandlerError (ErrorMessage msg [msg] Error) GlobalTag

handlerError ::
  Member (Stop HandlerError) r =>
  HasCallStack =>
  ErrorMessage ->
  HandlerTag ->
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
  HandlerTag ->
  e ->
  HandlerError
toHandlerError htag e =
  HandlerError (toErrorMessage e) htag

handlerErrorFrom ::
  ∀ e r a .
  ToErrorMessage e =>
  Member (Stop HandlerError) r =>
  HandlerTag ->
  Sem (Stop e : r) a ->
  Sem r a
handlerErrorFrom t =
  mapStop (toHandlerError t)

mapHandlerError ::
  ∀ e r a .
  ToErrorMessage e =>
  Member (Stop HandlerError) r =>
  Sem (Stop e : r) a ->
  Sem r a
mapHandlerError =
  mapStop (toHandlerError GlobalTag)

resumeHandlerErrorFrom ::
  ∀ eff e r a .
  ToErrorMessage e =>
  Members [eff !! e, Stop HandlerError] r =>
  HandlerTag ->
  Sem (eff : r) a ->
  Sem r a
resumeHandlerErrorFrom t =
  resumeHoist (toHandlerError t)

resumeHandlerError ::
  ∀ eff e r a .
  ToErrorMessage e =>
  Members [eff !! e, Stop HandlerError] r =>
  Sem (eff : r) a ->
  Sem r a
resumeHandlerError =
  resumeHandlerErrorFrom GlobalTag
