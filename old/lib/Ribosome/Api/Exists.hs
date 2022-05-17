module Ribosome.Api.Exists where

import Prettyprinter (viaShow, (<+>))

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode, fromMsgpack)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Api.Effect (vimCallFunction)
import Ribosome.System.Time (epochSeconds, sleep)

data Retry =
  Retry Int Double
  deriving stock (Show)

instance Default Retry where
  def = Retry 3 0.1

retry ::
  MonadIO f =>
  f a ->
  (a -> f (Either c b)) ->
  Retry ->
  f (Either c b)
retry thunk check (Retry timeout interval) = do
  start <- epochSeconds
  step start
  where
    step start = do
      result <- thunk
      checked <- check result
      recurse start checked
    recurse _ (Right b) = pure (Right b)
    recurse start (Left e) = do
      current <- epochSeconds
      if (current - start) < timeout
      then do
        sleep interval
        step start
      else pure $ Left e

waitFor ::
  Member Rpc r =>
  MonadIO m =>
  m Object ->
  (Object -> m (Either Text b)) ->
  Retry ->
  m (Either Text b)
waitFor thunk check' =
  retry thunk check
  where
    check result =
      case fromMsgpack result of
        Right a -> check' a
        Left e -> pure $ Left e

existsResult :: Object -> Either Text ()
existsResult (ObjectInt 1) = Right ()
existsResult a =
  Left $ "weird pure type " <+> viaShow a

vimExists ::
  Member Rpc r =>
  Text ->
  m Object
vimExists entity =
  vimCallFunction "exists" [toMsgpack entity]

vimDoesExist ::
  Member Rpc r =>
  Text ->
  m Bool
vimDoesExist entity =
  fmap (isRight . existsResult) (vimExists entity)

function ::
  Member Rpc r =>
  Text ->
  m Bool
function name =
  vimDoesExist ("*" <> name)

waitForFunction ::
  Member Rpc r =>
  MonadIO m =>
  Text ->
  Retry ->
  m (Either Text ())
waitForFunction name =
  waitFor thunk (pure . existsResult)
  where
    thunk = vimExists ("*" <> name)

waitForFunctionResult ::
  Member Rpc r =>
  MonadIO m =>
  Eq a =>
  Show a =>
  MsgpackDecode a =>
  Text ->
  a ->
  Retry ->
  m (Either Text ())
waitForFunctionResult name a retry' =
  waitForFunction name retry' >>= \case
    Right _ -> waitFor thunk (pure . check . fromMsgpack) retry'
    Left e -> pure (Left e)
  where
    thunk = vimCallFunction name []
    check (Right a') | a == a' =
      Right ()
    check (Right a') =
      Left $ "results differ:" <+> show a <+> "/" <+> show a'
    check (Left e) =
      Left $ "weird pure type: " <+> e
