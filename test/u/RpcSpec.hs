{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

module RpcSpec (htf_thisModulesTests) where

import qualified Data.Map as Map (empty)
import Language.Haskell.TH hiding (reportError)
import Neovim (CommandArguments, Neovim, Plugin(..))
import Test.Framework

import Ribosome.Api.Variable (setVar)
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Error.Report (reportError)
import Ribosome.Log (logError)
import Ribosome.Msgpack.Encode (MsgpackEncode(toMsgpack))
import Ribosome.Nvim.Api.IO (vimCallFunction, vimCommand, vimGetVar)
import Ribosome.Nvim.Api.RpcCall (RpcError)
import Ribosome.Plugin (RpcDef(RpcDef), cmd, riboPlugin, rpcHandler, sync)
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
    $(rpcHandler (sync . cmd []) 'handlerCmdOneArg)
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
successCommand cmd = do
  setVar resultVar (0 :: Int)
  vimCommand cmd
  gassertEqual target =<< vimGetVar resultVar

failureCommand ::
  AssertM m =>
  NvimE e m =>
  Text ->
  m ()
failureCommand cmd = do
  setVar resultVar (0 :: Int)
  catchAt recover (vimCommand cmd)
  gassertEqual (0 :: Int) =<< vimGetVar resultVar
  where
    recover (_ :: RpcError) =
      return ()

rpcSpec :: ExceptT RpcError (Neovim ()) ()
rpcSpec = do
  successCommand "HandlerCmdCmdArgs a b c"
  successCommand "HandlerCmdNoCmdArgs a b c"
  successCommand "HandlerCmdNoArgs"
  successCommand "HandlerCmdOneArg a"
  failureCommand "HandlerCmdCmdArgs a b"
  failureCommand "HandlerCmdNoCmdArgs a b"
  failureCommand "HandlerCmdNoArgs a"
  sleep 1

test_rpc :: IO ()
test_rpc = do
  liftIO $ updateGlobalLogger "test" (setLevel DEBUG)
  plug <- plugin
  -- traverse_ putStrLn $ lines $(stringE . pprint =<< rpcHandler (sync . cmd []) 'handlerCmdOneArg)
  integrationSpecDef plug rpcSpec
