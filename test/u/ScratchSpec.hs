{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

module ScratchSpec (htf_thisModulesTests) where

import Data.Default (def)
import qualified Data.Map as Map (toList)
import Neovim (Neovim, Plugin(..))
import Test.Framework

import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE, pluginInternalL)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import qualified Ribosome.Control.Ribosome as Ribosome (scratch)
import Ribosome.Data.ScratchOptions (ScratchOptions(ScratchOptions))
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (vimCallFunction, vimCommand)
import Ribosome.Nvim.Api.RpcCall (RpcError)
import Ribosome.Plugin (riboPlugin, rpcHandler, rpcHandlerDef, sync)
import Ribosome.Scratch (showInScratch)
import Ribosome.Test.Await (await)
import Ribosome.Test.Embed (integrationSpecDef)
import TestError (handleTestError)

target :: [Text]
target = ["line 1", "line 2"]

name :: Text
name =
  "buffi"

makeScratch ::
  MonadDeepError e DecodeError m =>
  MonadRibo m =>
  NvimE e m =>
  m ()
makeScratch =
  void $ showInScratch target (ScratchOptions False True False True (Just 0) [] [] name)

scratchCount ::
  MonadRibo m =>
  m Int
scratchCount =
  length . Map.toList <$> pluginInternalL Ribosome.scratch

$(return [])

scratchPlugin :: IO (Plugin (Ribosome ()))
scratchPlugin = do
  env <- newRibosome "test" ()
  return $ riboPlugin "test" env funcs [] handleTestError def
  where
    funcs = [$(rpcHandlerDef 'makeScratch), $(rpcHandler sync 'scratchCount)]

scratchSpec :: ExceptT RpcError (Neovim ()) ()
scratchSpec = do
  () <- vimCallFunction "MakeScratch" []
  await (gassertEqual (1 :: Int)) scratches
  await (gassertEqual target) currentBufferContent
  vimCommand "bdelete"
  await (gassertEqual 0) scratches
  where
    scratches = vimCallFunction "ScratchCount" []

test_scratch :: IO ()
test_scratch = do
  plug <- scratchPlugin
  integrationSpecDef plug scratchSpec
