module Ribosome.Api.Exists(
  retry,
  waitForFunction,
  vimDoesExist,
  function,
) where

import Control.Monad.IO.Class (MonadIO)
import Data.Default (Default(def))
import Data.Either (isRight)
import Data.Text.Prettyprint.Doc (prettyList, viaShow, (<+>))
import Neovim (
  AnsiStyle,
  Doc,
  Neovim,
  NvimObject,
  Object(ObjectInt),
  fromObject,
  toObject,
  vim_call_function',
  )

import Ribosome.System.Time (epochSeconds, sleep)

data Retry =
  Retry Int Double
  deriving Show

instance Default Retry where
  def = Retry 3 0.1

retry :: MonadIO f => f a -> (a -> f (Either c b)) -> Retry -> f (Either c b)
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

waitFor :: NvimObject b =>
  Neovim e Object ->
  (Object -> Neovim e (Either (Doc AnsiStyle) b)) ->
  Retry ->
  Neovim e (Either (Doc AnsiStyle) b)
waitFor thunk check' =
  retry thunk check
  where
    check result =
      case fromObject result of
        Right a -> check' a
        Left e -> return $ Left e

existsResult :: Object -> Either (Doc AnsiStyle) ()
existsResult (ObjectInt 1) = Right ()
existsResult a = Left $ prettyList "weird return type " <+> viaShow a

vimExists :: String -> Neovim e Object
vimExists entity =
  vim_call_function' "exists" [toObject entity]

vimDoesExist :: String -> Neovim e Bool
vimDoesExist entity =
  fmap (isRight . existsResult) (vimExists entity)

function :: String -> Neovim e Bool
function name =
  vimDoesExist ("*" ++ name)

waitForFunction :: String -> Retry -> Neovim e (Either (Doc AnsiStyle) ())
waitForFunction name =
  waitFor thunk (return . existsResult)
  where
    thunk = vimExists ("*" ++ name)
