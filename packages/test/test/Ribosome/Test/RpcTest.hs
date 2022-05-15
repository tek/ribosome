module Ribosome.Test.RpcTest where

import Data.DeepPrisms (DeepPrisms)
import qualified Data.List as List (lines)
import qualified Data.Map.Strict as Map (empty)
import Hedgehog ((===))
import Language.Haskell.TH hiding (reportError)
import Neovim (CommandArguments, Plugin(..))
import System.Log.Logger (Priority(DEBUG), setLevel, updateGlobalLogger)
import TestError (RiboTest, TestError)

import Ribosome.Api.Variable (setVar)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Error.Report (reportError)
import Ribosome.Host.Api.Effect (nvimCommand, vimGetVar)
import Ribosome.Plugin (RpcDef, cmd, riboPlugin, rpcHandler, sync)
import Ribosome.System.Time (sleep)
import Ribosome.Test.Embed (integrationTestDef)
import Ribosome.Test.Run (UnitTest)

handleError ::
  DeepPrisms e RpcError =>
  TestError ->
  Ribo s e ()
handleError =
  reportError "test"

target :: Int
target =
  13

resultVar :: Text
resultVar =
  "test_result"

handler ::
  Member Rpc r =>
  m Int
handler = do
  setVar resultVar target
  return target

handlerCmdCmdArgs ::
  Member Rpc r =>
  CommandArguments ->
  Text ->
  Text ->
  Text ->
  m Int
handlerCmdCmdArgs _ _ _ _ =
  handler

handlerCmdNoCmdArgs ::
  Member Rpc r =>
  Text ->
  Text ->
  Text ->
  m Int
handlerCmdNoCmdArgs _ _ _ =
  handler

handlerCmdNoArgs ::
  Member Rpc r =>
  m Int
handlerCmdNoArgs =
  handler

handlerCmdOneArg ::
  Member Rpc r =>
  Text ->
  m Int
handlerCmdOneArg _ =
  handler

handlerCmdMaybeArg ::
  Member Rpc r =>
  Maybe Text ->
  m Int
handlerCmdMaybeArg _ =
  handler

handlerCmdListArg ::
  Member Rpc r =>
  [Text] ->
  m Int
handlerCmdListArg _ =
  handler

$(return [])

handlers ::
  Member Rpc r =>
  [[RpcDef m]]
handlers =
  [
    $(rpcHandler (sync . cmd []) 'handlerCmdCmdArgs),
    $(rpcHandler (sync . cmd []) 'handlerCmdNoCmdArgs),
    $(rpcHandler (sync . cmd []) 'handlerCmdNoArgs),
    $(rpcHandler (sync . cmd []) 'handlerCmdOneArg),
    $(rpcHandler (sync . cmd []) 'handlerCmdMaybeArg),
    $(rpcHandler (sync . cmd []) 'handlerCmdListArg)
    ]

plugin :: IO (Plugin (Ribosome ()))
plugin = do
  ribo <- newRibosome "test" def
  pure $ riboPlugin "test" ribo handlers [] handleError Map.empty

successCommand ::
  Text ->
  RiboTest ()
successCommand cmd' = do
  setVar resultVar (0 :: Int)
  nvimCommand cmd'
  (target ===) =<< vimGetVar resultVar

failureCommand ::
  Text ->
  RiboTest ()
failureCommand cmd' = do
  lift (setVar resultVar (0 :: Int))
  catchAt recoverRpc (nvimCommand cmd')
  ((0 :: Int) ===) =<< vimGetVar resultVar
  where
    recoverRpc (_ :: RpcError) =
      return ()

rpcTest :: RiboTest ()
rpcTest = do
  successCommand "HandlerCmdCmdArgs a b c"
  successCommand "HandlerCmdNoCmdArgs a b c"
  successCommand "HandlerCmdNoArgs"
  successCommand "HandlerCmdOneArg a"
  successCommand "HandlerCmdMaybeArg a"
  successCommand "HandlerCmdMaybeArg"
  successCommand "HandlerCmdListArg"
  successCommand "HandlerCmdListArg a b c"
  failureCommand "HandlerCmdCmdArgs a b"
  failureCommand "HandlerCmdNoCmdArgs a b"
  failureCommand "HandlerCmdNoArgs a"
  sleep 1

test_rpc :: UnitTest
test_rpc = do
  liftIO $ updateGlobalLogger "test" (setLevel DEBUG)
  plug <- liftIO plugin
  when debug $ traverse_ putStrLn . List.lines $ $(stringE . pprint =<< rpcHandler (sync . cmd []) 'handlerCmdListArg)
  integrationTestDef plug rpcTest
  where
    debug =
      False
