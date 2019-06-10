{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

module AutocmdSpec (htf_thisModulesTests) where

import Data.Default (def)
import Neovim (Plugin(..))
import Test.Framework

import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Msgpack.Encode (MsgpackEncode(toMsgpack))
import Ribosome.Nvim.Api.IO (vimCommand, vimGetVar, vimSetVar)
import Ribosome.Orphans ()
import Ribosome.Plugin (autocmd, riboPlugin, rpcHandler)
import Ribosome.Test.Await (await)
import Ribosome.Test.Embed (integrationSpecDef)
import Ribosome.Test.Orphans ()
import TestError (RiboT, handleTestError)

varName :: Text
varName =
  "result"

result :: Int
result =
  5

testAuto ::
  NvimE e m =>
  MonadRibo m =>
  m ()
testAuto =
  vimSetVar varName (toMsgpack result)

$(return [])

autocmdPlugin :: IO (Plugin (Ribosome ()))
autocmdPlugin = do
  env <- newRibosome "test" ()
  return $ riboPlugin "test" env funcs [] handleTestError def
  where
    funcs = [$(rpcHandler (autocmd "BufWritePre") 'testAuto)]

autocmdSpec :: RiboT ()
autocmdSpec = do
  () <- vimCommand "doautocmd BufWritePre"
  await (gassertEqual result) (vimGetVar varName)

test_autocmd :: IO ()
test_autocmd = do
  plug <- autocmdPlugin
  integrationSpecDef plug autocmdSpec
