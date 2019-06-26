{-# OPTIONS_GHC -F -pgmF htfpp #-}

module RpcSpec (htf_thisModulesTests) where

import qualified Data.Map.Strict as Map (empty)
-- import Language.Haskell.TH hiding (reportError)
import Neovim (CommandArguments, Plugin(..))
import Test.Framework

import Ribosome.Api.Variable (setVar)
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Error.Report (reportError)
import Ribosome.Nvim.Api.IO (vimCommand, vimGetVar)
import Ribosome.Nvim.Api.RpcCall (RpcError)
import Ribosome.Plugin (RpcDef, cmd, riboPlugin, rpcHandler, sync)
import Ribosome.System.Time (sleep)
import Ribosome.Test.Embed (integrationSpecDef)
import System.Log.Logger (Priority(DEBUG), setLevel, updateGlobalLogger)
import TestError (RiboT, TestError)

handleError :: TestError -> RiboT ()
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
  MonadRibo m =>
  NvimE e m =>
  CommandArguments ->
  Text ->
  Text ->
  Text ->
  m Int
handlerCmdCmdArgs _ _ _ _ =
  handler

handlerCmdNoCmdArgs ::
  MonadRibo m =>
  NvimE e m =>
  Text ->
  Text ->
  Text ->
  m Int
handlerCmdNoCmdArgs _ _ _ =
  handler

handlerCmdNoArgs ::
  MonadRibo m =>
  NvimE e m =>
  m Int
handlerCmdNoArgs =
  handler

handlerCmdOneArg ::
  MonadRibo m =>
  NvimE e m =>
  Text ->
  m Int
handlerCmdOneArg _ =
  handler

handlerCmdMaybeArg ::
  MonadRibo m =>
  NvimE e m =>
  Maybe Text ->
  m Int
handlerCmdMaybeArg _ =
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
    $(rpcHandler (sync . cmd []) 'handlerCmdMaybeArg)
    ]


plugin :: IO (Plugin (Ribosome ()))
plugin = do
  ribo <- newRibosome "test" def
  return $ riboPlugin "test" ribo handlers [] handleError Map.empty

successCommand ::
  AssertM m =>
  NvimE e m =>
  Text ->
  m ()
successCommand cmd' = do
  setVar resultVar (0 :: Int)
  vimCommand cmd'
  gassertEqual target =<< vimGetVar resultVar

failureCommand ::
  AssertM m =>
  NvimE e m =>
  Text ->
  m ()
failureCommand cmd' = do
  setVar resultVar (0 :: Int)
  catchAt recoverRpc (vimCommand cmd')
  gassertEqual (0 :: Int) =<< vimGetVar resultVar
  where
    recoverRpc (_ :: RpcError) =
      return ()

rpcSpec :: RiboT ()
rpcSpec = do
  successCommand "HandlerCmdCmdArgs a b c"
  successCommand "HandlerCmdNoCmdArgs a b c"
  successCommand "HandlerCmdNoArgs"
  successCommand "HandlerCmdOneArg a"
  successCommand "HandlerCmdMaybeArg a"
  successCommand "HandlerCmdMaybeArg"
  failureCommand "HandlerCmdCmdArgs a b"
  failureCommand "HandlerCmdNoCmdArgs a b"
  failureCommand "HandlerCmdNoArgs a"
  sleep 1

test_rpc :: IO ()
test_rpc = do
  liftIO $ updateGlobalLogger "test" (setLevel DEBUG)
  plug <- plugin
  -- traverse_ putStrLn $ take 1 $ lines $(stringE . pprint =<< rpcHandler (sync . cmd []) 'handlerCmdMaybeArg)
  integrationSpecDef plug rpcSpec
