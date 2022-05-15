module Ribosome.Test.Exists where

import Data.MessagePack (Object(ObjectInt))
import Neovim.API.Text (vim_call_function)
import UnliftIO.Exception (tryAny)

import Ribosome.Data.Text (capitalize)
import Ribosome.System.Time (epochSeconds, sleep)

retry ::
  MonadIO m =>
  MonadFail m =>
  Double ->
  Int ->
  m a ->
  (a -> m (Either Text b)) ->
  m b
retry interval timeout thunk check = do
  start <- epochSeconds
  step start
  where
    step start = do
      result <- thunk
      checked <- check result
      recurse start checked
    recurse _ (Right b) = return b
    recurse start (Left e) = do
      current <- epochSeconds
      if (current - start) < timeout
      then do
        sleep interval
        step start
      else fail (toString e)

waitForPlugin :: Text -> Double -> Int -> Neovim env ()
waitForPlugin name interval timeout =
  retry interval timeout thunk check
  where
    thunk = tryAny $ vim_call_function "exists" $ fromList [toObject $ "*" <> capitalize name <> "Poll"]
    check (Right (ObjectInt 1)) = return $ Right ()
    check (Right a) = return $ Left $ errormsg <> "weird return type " <> show a
    check (Left e) = return $ Left $ errormsg <> show e
    errormsg = "plugin didn't start within " <> show timeout <> " seconds: "
