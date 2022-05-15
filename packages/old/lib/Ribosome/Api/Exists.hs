module Ribosome.Api.Exists where

import Prettyprinter (viaShow, (<+>))

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Msgpack.Decode (MsgpackDecode, fromMsgpack)
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Nvim.Api.IO (vimCallFunction)
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
    recurse _ (Right b) = return (Right b)
    recurse start (Left e) = do
      current <- epochSeconds
      if (current - start) < timeout
      then do
        sleep interval
        step start
      else return $ Left e

waitFor ::
  NvimE e m =>
  MonadIO m =>
  m Object ->
  (Object -> m (Either (Doc AnsiStyle) b)) ->
  Retry ->
  m (Either (Doc AnsiStyle) b)
waitFor thunk check' =
  retry thunk check
  where
    check result =
      case fromMsgpack result of
        Right a -> check' a
        Left e -> return $ Left e

existsResult :: Object -> Either (Doc AnsiStyle) ()
existsResult (ObjectInt 1) = Right ()
existsResult a =
  Left $ "weird return type " <+> viaShow a

vimExists ::
  NvimE e m =>
  Text ->
  m Object
vimExists entity =
  vimCallFunction "exists" [toMsgpack entity]

vimDoesExist ::
  NvimE e m =>
  Text ->
  m Bool
vimDoesExist entity =
  fmap (isRight . existsResult) (vimExists entity)

function ::
  NvimE e m =>
  Text ->
  m Bool
function name =
  vimDoesExist ("*" <> name)

waitForFunction ::
  NvimE e m =>
  MonadIO m =>
  Text ->
  Retry ->
  m (Either (Doc AnsiStyle) ())
waitForFunction name =
  waitFor thunk (return . existsResult)
  where
    thunk = vimExists ("*" <> name)

waitForFunctionResult ::
  NvimE e m =>
  MonadIO m =>
  Eq a =>
  Show a =>
  MsgpackDecode a =>
  Text ->
  a ->
  Retry ->
  m (Either (Doc AnsiStyle) ())
waitForFunctionResult name a retry' =
  waitForFunction name retry' >>= \case
    Right _ -> waitFor thunk (return . check . fromMsgpack) retry'
    Left e -> return (Left e)
  where
    thunk = vimCallFunction name []
    check (Right a') | a == a' =
      Right ()
    check (Right a') =
      Left $ "results differ:" <+> show a <+> "/" <+> show a'
    check (Left e) =
      Left $ "weird return type: " <+> e
