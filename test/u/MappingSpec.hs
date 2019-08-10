{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

module MappingSpec (htf_thisModulesTests) where

import qualified Data.Map.Strict as Map (empty)
import Neovim (Plugin(..))
import Test.Framework

import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Data.Mapping (Mapping(Mapping), MappingIdent(MappingIdent))
import Ribosome.Data.ScratchOptions (ScratchOptions(ScratchOptions))
import Ribosome.Msgpack.Encode (MsgpackEncode(toMsgpack))
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (nvimFeedkeys, vimCallFunction, vimGetVar, vimSetVar)
import Ribosome.Plugin (riboPlugin, rpcHandlerDef)
import Ribosome.Plugin.Mapping (MappingHandler, mappingHandler)
import Ribosome.Scratch (showInScratch)
import Ribosome.Test.Await (await)
import Ribosome.Test.Embed (integrationSpecDef)
import Ribosome.Test.Orphans ()
import TestError (RiboT, handleTestError)

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
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  m ()
setupMappingScratch = do
  _ <- showInScratch target options
  return ()
  where
    options =
      ScratchOptions False True False True True True False Nothing Nothing Nothing [] [mapping] "buffi"

$(return [])

mappingPlugin :: IO (Plugin (Ribosome ()))
mappingPlugin = do
  env <- newRibosome "mapping" ()
  return $ riboPlugin "mapping" env funcs [mapHandler] handleTestError Map.empty
  where
    funcs = [$(rpcHandlerDef 'setupMappingScratch)]

mappingSpec :: RiboT ()
mappingSpec = do
  () <- vimCallFunction "SetupMappingScratch" []
  await (gassertEqual target) currentBufferContent
  nvimFeedkeys "a" "x" False
  await (gassertEqual (13 :: Int)) (vimGetVar "number")

test_mapping :: IO ()
test_mapping = do
  plug <- mappingPlugin
  integrationSpecDef plug mappingSpec
