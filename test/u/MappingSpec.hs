{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

module MappingSpec (htf_thisModulesTests) where

import Control.Monad.Trans.Except (ExceptT)
import Data.DeepPrisms (deepPrisms)
import qualified Data.Map as Map (empty)
import Neovim (Neovim, Plugin(..))
import Test.Framework

import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE, RiboN)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Data.Mapping (Mapping(Mapping), MappingError, MappingIdent(MappingIdent))
import Ribosome.Data.ScratchOptions (ScratchOptions(ScratchOptions))
import Ribosome.Msgpack.Encode (MsgpackEncode(toMsgpack))
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (nvimFeedkeys, vimCallFunction, vimGetVar, vimSetVar)
import Ribosome.Nvim.Api.RpcCall (RpcError)
import Ribosome.Plugin (riboPlugin, rpcHandlerDef)
import Ribosome.Plugin.Mapping (MappingHandler, mappingHandler)
import Ribosome.Scratch (showInScratch)
import Ribosome.Test.Await (await)
import Ribosome.Test.Embed (integrationSpecDef)
import Ribosome.Test.Orphans ()
import TestError (handleTestError)

target :: [Text]
target = ["line 1", "line 2"]

go :: NvimE e m => m ()
go =
  vimSetVar "number" (toMsgpack (13 :: Int))

mapping :: Mapping
mapping =
  Mapping (MappingIdent "go") "a" "n" False True

mapHandler :: NvimE e m => MappingHandler m
mapHandler =
  mappingHandler "go" go

setupMappingScratch ::
  MonadDeepError e DecodeError m =>
  MonadRibo m =>
  NvimE e m =>
  m ()
setupMappingScratch = do
  _ <- showInScratch target (ScratchOptions False True False True (Just 0) [] [mapping] "buffi")
  return ()

$(return [])

mappingPlugin :: IO (Plugin (Ribosome ()))
mappingPlugin = do
  env <- newRibosome "mapping" ()
  return $ riboPlugin "mapping" env funcs [mapHandler] handleTestError Map.empty
  where
    funcs = [$(rpcHandlerDef 'setupMappingScratch)]

mappingSpec :: ExceptT RpcError (Neovim ()) ()
mappingSpec = do
  () <- vimCallFunction "SetupMappingScratch" []
  await (gassertEqual target) currentBufferContent
  nvimFeedkeys "a" "x" False
  await (gassertEqual (13 :: Int)) (vimGetVar "number")

test_mapping :: IO ()
test_mapping = do
  plug <- mappingPlugin
  integrationSpecDef plug mappingSpec
