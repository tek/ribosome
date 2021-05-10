module Ribosome.Test.AutocmdTest where

import Hedgehog ((===))
import Neovim (Plugin(..))
import TestError (RiboTest, handleTestError)

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Msgpack.Encode (MsgpackEncode(toMsgpack))
import Ribosome.Nvim.Api.IO (vimCommand, vimGetVar, vimSetVar)
import Ribosome.Orphans ()
import Ribosome.Plugin (autocmd, riboPlugin, rpcHandler)
import Ribosome.Test.Await (await)
import Ribosome.Test.Embed (integrationTestDef)
import Ribosome.Test.Orphans ()
import Ribosome.Test.Run (UnitTest)

varName :: Text
varName =
  "result"

result :: Int
result =
  5

testAuto ::
  NvimE e m =>
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

autocmdTest :: RiboTest ()
autocmdTest = do
  () <- vimCommand "doautocmd BufWritePre"
  await (result ===) (vimGetVar varName)

test_autocmd :: UnitTest
test_autocmd = do
  plug <- liftIO autocmdPlugin
  integrationTestDef plug autocmdTest
