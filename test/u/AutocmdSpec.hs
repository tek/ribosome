{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

module AutocmdSpec(
  htf_thisModulesTests
) where

import Control.Monad.Trans.Except (ExceptT)
import Data.DeepPrisms (deepPrisms)
import Neovim (Neovim, Plugin(..))
import Test.Framework

import Ribosome.Control.Monad.Ribo (ConcNvimS, MonadRibo, NvimE, RiboE)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Data.Mapping (MappingError)
import Ribosome.Msgpack.Encode (MsgpackEncode(toMsgpack))
import Ribosome.Nvim.Api.IO (vimCommand, vimGetVar, vimSetVar)
import Ribosome.Nvim.Api.RpcCall (RpcError)
import Ribosome.Orphans ()
import Ribosome.Plugin (autocmd, nvimPlugin, rpcHandler)
import Ribosome.Test.Await (await)
import Ribosome.Test.Embed (integrationSpecDef)
import Ribosome.Test.Orphans ()

data AutocmdTestError =
  Rpc RpcError
  |
  Mapp MappingError

deepPrisms ''AutocmdTestError

varName :: String
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

handleError :: AutocmdTestError -> RiboE () AutocmdTestError (ConcNvimS ()) ()
handleError _ =
  return ()

autocmdPlugin :: IO (Plugin (Ribosome ()))
autocmdPlugin = do
  env <- newRibosome "test" ()
  return $ nvimPlugin "test" env funcs [] handleError
  where
    funcs = [$(rpcHandler (autocmd "BufWritePre") 'testAuto)]

autocmdSpec :: ExceptT RpcError (Neovim ()) ()
autocmdSpec = do
  () <- vimCommand "doautocmd BufWritePre"
  await (gassertEqual result) (vimGetVar varName)

test_autocmd :: IO ()
test_autocmd = do
  plug <- autocmdPlugin
  integrationSpecDef plug autocmdSpec
