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
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE, Ribo)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Error.Report (reportError)
import Ribosome.Nvim.Api.IO (vimCommand, vimGetVar)
import Ribosome.Nvim.Api.RpcCall (RpcError)
import Ribosome.Plugin (RpcDef, cmd, riboPlugin, rpcHandler, sync)
import Ribosome.System.Time (sleep)
import Ribosome.Test.Embed (integrationSpecDef)
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
  NvimE e m =>
  m Int
handler = do
  setVar resultVar target
  return target

handlerCmdCmdArgs ::
  NvimE e m =>
  CommandArguments ->
  Text ->
  Text ->
  Text ->
  m Int
handlerCmdCmdArgs _ _ _ _ =
  handler

handlerCmdNoCmdArgs ::
  NvimE e m =>
  Text ->
  Text ->
  Text ->
  m Int
handlerCmdNoCmdArgs _ _ _ =
  handler

handlerCmdNoArgs ::
  NvimE e m =>
  m Int
handlerCmdNoArgs =
  handler

handlerCmdOneArg ::
  NvimE e m =>
  Text ->
  m Int
handlerCmdOneArg _ =
  handler

handlerCmdMaybeArg ::
  NvimE e m =>
  Maybe Text ->
  m Int
handlerCmdMaybeArg _ =
  handler

handlerCmdListArg ::
  NvimE e m =>
  [Text] ->
  m Int
handlerCmdListArg _ =
  handler

$(return [])

handlers ::
  MonadRibo m =>
  NvimE e m =>
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
  vimCommand cmd'
  (target ===) =<< vimGetVar resultVar

failureCommand ::
  Text ->
  RiboTest ()
failureCommand cmd' = do
  lift (setVar resultVar (0 :: Int))
  catchAt recoverRpc (vimCommand cmd')
  ((0 :: Int) ===) =<< vimGetVar resultVar
  where
    recoverRpc (_ :: RpcError) =
      return ()

rpcSpec :: RiboTest ()
rpcSpec = do
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
  integrationSpecDef plug rpcSpec
  where
    debug =
      False
