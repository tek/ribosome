module Ribosome.Host.Data.HandlerError where

import Exon (exon)
import Fcf (Pure1, type (@@))
import Fcf.Class.Functor (FMap)
import Polysemy.Log (Severity (Error))
import Prelude hiding (tag)
import Text.Show (showParen, showsPrec)

-- |The provenance of an error, for use in error logs.
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

-- |Convert a 'HandlerTag' to 'Text' by using @"global"@ for 'GlobalTag'.
handlerTagName :: HandlerTag -> Text
handlerTagName = \case
  GlobalTag -> "global"
  HandlerTag t -> t

-- |An error with different messages intended to be sent to Neovim and the log, respectively.
--
-- Also contains the 'Severity' of the error, or minimum log level, which determines whether the error should be logged,
-- and what kind of highlighting should be used in Neovim (red for errors, orange for warnings, none for infomrational
-- errors).
--
-- The log message may span multiple lines.
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

-- |The error type used by request handlers and expected by the RPC dispatcher.
data HandlerError :: Type where
  HandlerError :: HasCallStack => {
    msg :: ErrorMessage,
    tag :: HandlerTag
  } -> HandlerError

instance Show HandlerError where
  showsPrec d HandlerError {..} =
    showParen (d > 10) [exon|HandlerError { msg = #{showsPrec 0 msg}, tag = #{showsPrec 0 tag} }|]

-- |Construct a 'HandlerError' from a simple 'Text' using 'GlobalTag'.
simple ::
  HasCallStack =>
  Text ->
  HandlerError
simple msg =
  withFrozenCallStack do
    HandlerError (ErrorMessage msg [msg] Error) GlobalTag

-- |Stop with 'HandlerError'.
handlerError ::
  Member (Stop HandlerError) r =>
  HasCallStack =>
  ErrorMessage ->
  HandlerTag ->
  Sem r a
handlerError msg tag =
  withFrozenCallStack do
    stop (HandlerError msg tag)

-- |Stop with 'HandlerError'.
basicHandlerError ::
  Member (Stop HandlerError) r =>
  HasCallStack =>
  Text ->
  [Text] ->
  Sem r a
basicHandlerError user log =
  withFrozenCallStack do
    handlerError (ErrorMessage user log Error) GlobalTag

instance IsString HandlerError where
  fromString =
    simple . toText

-- |The class of errors that are known to be used as a 'HandlerError'.
--
-- This is used to create a uniform format for handlers, since control flow is passed on to the internal machinery when
-- they return.
-- If an error would be thrown that is not caught by the request dispatcher, the entire plugin would stop, so all 'Stop'
-- and 'Resumable' effects need to be converted to 'HandlerError' before returning (see [Errors]("Ribosome#errors")).
--
-- The combinators associated with this class make this task a little less arduous:
--
-- > data NumbersError = InvalidNumber
-- >
-- > instance ToErrorMessage NumbersError where
-- >   toErrorMessage InvalidNumber = ErrorMesssage "Invalid number!" ["The user entered an invalid number"] Warn
-- >
-- > count :: Int -> Sem r Int
-- > count i =
-- >   resumeHandlerError @Rpc $ mapHandlerError @NumbersError do
-- >     when (i == 0) (stop InvalidNumber)
-- >     nvimGetVar ("number_" <> show i)
--
-- Here 'resumeHandlerError' converts a potential 'RpcError' from 'Ribosome.Api.nvimGetVar' to 'HandlerError' (e.g. if
-- the variable is not set), while 'mapHandlerError' uses the instance @'ToErrorMessage' 'NumbersError'@ to convert the
-- call to 'stop'.
--
-- These combinators have variants suffixed with @From@, which allow specifying a 'HandlerTag' to identify which
-- component of a plugin caused the error, visible in the logs.
class ToErrorMessage e where
  toErrorMessage :: e -> ErrorMessage

instance ToErrorMessage ErrorMessage where
  toErrorMessage =
    id

instance ToErrorMessage Void where
  toErrorMessage = \case

-- |Construct a 'HandlerError' from an instance of 'ToErrorMessage' and a 'HandlerTag'.
toHandlerError ::
  ToErrorMessage e =>
  HandlerTag ->
  e ->
  HandlerError
toHandlerError htag e =
  HandlerError (toErrorMessage e) htag

-- |Reinterpret @'Stop' err@ to @'Stop' 'HandlerError'@ if @err@ is an instance of 'ToErrorMessage'.
-- Takes a 'HandlerTag' identifying the component that threw the error.
mapHandlerErrorFrom ::
  ∀ e r a .
  ToErrorMessage e =>
  Member (Stop HandlerError) r =>
  HandlerTag ->
  Sem (Stop e : r) a ->
  Sem r a
mapHandlerErrorFrom t =
  mapStop (toHandlerError t)

-- |Reinterpret @'Stop' err@ to @'Stop' 'HandlerError'@ if @err@ is an instance of 'ToErrorMessage'.
-- Uses 'GlobalTag'.
mapHandlerError ::
  ∀ e r a .
  ToErrorMessage e =>
  Member (Stop HandlerError) r =>
  Sem (Stop e : r) a ->
  Sem r a
mapHandlerError =
  mapHandlerErrorFrom GlobalTag

type Stops errs =
  FMap (Pure1 Stop) Fcf.@@ errs

-- |Map multiple errors to 'HandlerError'.
class MapHandlerErrors (errs :: [Type]) (r :: EffectRow) where
  -- |Map multiple errors to 'HandlerError'.
  -- This needs the errors specified as type applications.
  --
  -- > mapHandlerErrors @[RpcError, SettingError]
  mapHandlerErrors :: InterpretersFor (Stops errs) r

instance MapHandlerErrors '[] r where
  mapHandlerErrors =
    id

instance (
    ToErrorMessage err,
    MapHandlerErrors errs r,
    Member (Stop HandlerError) (Stops errs ++ r)
  ) => MapHandlerErrors (err : errs) r where
    mapHandlerErrors =
      mapHandlerErrors @errs . mapHandlerError @err

-- |Convert the effect @eff@ to @'Resumable' err eff@ and @'Stop' 'HandlerError'@ if @err@ is an instance of
-- 'ToErrorMessage'.
-- Takes a 'HandlerTag' identifying the component that threw the error.
resumeHandlerErrorFrom ::
  ∀ eff e r a .
  ToErrorMessage e =>
  Members [eff !! e, Stop HandlerError] r =>
  HandlerTag ->
  Sem (eff : r) a ->
  Sem r a
resumeHandlerErrorFrom t =
  resumeHoist (toHandlerError t)

-- |Convert the effect @eff@ to @'Resumable' err eff@ and @'Stop' 'HandlerError'@ if @err@ is an instance of
-- 'ToErrorMessage'.
-- Uses 'GlobalTag'.
resumeHandlerError ::
  ∀ eff err r a .
  ToErrorMessage err =>
  Members [eff !! err, Stop HandlerError] r =>
  Sem (eff : r) a ->
  Sem r a
resumeHandlerError =
  resumeHandlerErrorFrom GlobalTag

-- |Resume multiple effects as 'HandlerError's.
class ResumeHandlerErrors (effs :: EffectRow) (errs :: [Type]) (r :: EffectRow) where
  -- |Resume multiple effects as 'HandlerError's.
  -- This needs both effects and errors specified as type applications (though only the shape for the errors).
  --
  -- > resumeHandlerErrors @[Rpc, Settings] @[_, _]
  resumeHandlerErrors :: InterpretersFor effs r

instance ResumeHandlerErrors '[] '[] r where
  resumeHandlerErrors =
    id

instance (
    ToErrorMessage err,
    ResumeHandlerErrors effs errs r,
    Members [eff !! err, Stop HandlerError] (effs ++ r)
  ) => ResumeHandlerErrors (eff : effs) (err : errs) r where
    resumeHandlerErrors =
      resumeHandlerErrors @effs @errs . resumeHandlerError @eff @err

-- |Extract the user message from an 'ErrorMessage'.
handlerErrorMessage :: HandlerError -> Text
handlerErrorMessage (HandlerError (ErrorMessage user log _) htag) =
  unlines ([exon|#{handlerTagName htag}:|] : user : log)

-- |Extract the user message from an instance of 'ToErrorMessage'.
userErrorMessage ::
  ∀ e .
  ToErrorMessage e =>
  e ->
  Text
userErrorMessage e =
  user
  where
    ErrorMessage {user} =
      toErrorMessage e

-- |Resume an effect with an error that's an instance of 'ToErrorMessage' by passing its user message to a function.
resumeHoistUserMessage ::
  ∀ err eff err' r .
  ToErrorMessage err =>
  Members [eff !! err, Stop err'] r =>
  (Text -> err') ->
  InterpreterFor eff r
resumeHoistUserMessage f =
  resumeHoist (f . userErrorMessage)

-- |Map an error that's an instance of 'ToErrorMessage' by passing its user message to a function.
mapUserMessage ::
  ∀ err err' r .
  ToErrorMessage err =>
  Member (Stop err') r =>
  (Text -> err') ->
  InterpreterFor (Stop err) r
mapUserMessage f =
  mapStop (f . userErrorMessage)

-- |Convert an error that's an instance of 'ToErrorMessage' to 'Fail', for use in tests.
stopHandlerToFail ::
  ∀ e r .
  Member Fail r =>
  ToErrorMessage e =>
  InterpreterFor (Stop e) r
stopHandlerToFail =
  either (fail . toString . userErrorMessage) pure <=< runStop
{-# inline stopHandlerToFail #-}

-- |Resume an effect with an error that's an instance of 'ToErrorMessage' by reinterpreting to 'Fail', for use in tests.
resumeHandlerFail ::
  ∀ eff err r .
  Members [Fail, eff !! err] r =>
  ToErrorMessage err =>
  InterpreterFor eff r
resumeHandlerFail =
  resuming (fail . toString . userErrorMessage)
{-# inline resumeHandlerFail #-}
