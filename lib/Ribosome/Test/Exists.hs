module Ribosome.Test.Exists(
  waitForPlugin,
) where

import Control.Monad.IO.Class
import Data.Strings (strCapitalize)
import Neovim

import Ribosome.Data.Time (epochSeconds, sleep)

retry :: MonadIO f => Double -> Int -> f a -> (a -> f (Either String b)) -> f b
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
      else fail e

waitForPlugin :: String -> Double -> Int -> Neovim env ()
waitForPlugin name interval timeout =
  retry interval timeout thunk check
  where
    thunk = vim_call_function "exists" [toObject $ "*" ++ strCapitalize name ++ "Poll"]
    check (Right (ObjectInt 1)) = return $ Right ()
    check (Right a) = return $ Left $ errormsg ++ "weird return type " ++ show a
    check (Left e) = return $ Left $ errormsg ++ show e
    errormsg = "plugin didn't start within " ++ show timeout ++ " seconds: "
