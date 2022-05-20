module Ribosome.Test.MappingTest where

import Polysemy.Test (UnitTest, (===))

import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Data.Mapping (Mapping (Mapping), MappingIdent (MappingIdent))
import Ribosome.Data.ScratchOptions (ScratchOptions (ScratchOptions))
import Ribosome.Host.Api.Effect (nvimFeedkeys, vimCallFunction, vimGetVar, vimSetVar)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Scratch (showInScratch)
import Ribosome.Test.Run (runTest, rpcError)
import Ribosome.Test.Wait (assertWait)
import Ribosome.Embed (embedNvimPlugin)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Handler (rpcFunction)
import Ribosome.Host.Data.Execution (Execution(Sync))
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Data.Scratch (Scratch)
import Ribosome.Data.PluginName (PluginName)

target :: [Text]
target = ["line 1", "line 2"]

mappingHandler ::
  Members [Rpc !! RpcError, Error HandlerError] r =>
  Sem r ()
mappingHandler =
  rpcError (vimSetVar "number" (13 :: Int))

mapping :: Mapping
mapping =
  Mapping (MappingIdent "mappingHandler") "a" "n" False True

setupMappingScratch ::
  Members [Rpc !! RpcError, AtomicState (Map Text Scratch), Reader PluginName, Error HandlerError, Log, Resource] r =>
  Sem r ()
setupMappingScratch = do
  rpcError (void (showInScratch target options))
  where
    options =
      ScratchOptions False True False True True True False Nothing Nothing Nothing [] [mapping] Nothing "mappo"

handlers ::
  ∀ r .
  Members [Rpc !! RpcError, AtomicState (Map Text Scratch), Reader PluginName, Log, Resource] r =>
  [RpcHandler r]
handlers =
  [
    rpcFunction "Setup" Sync (setupMappingScratch @(Error HandlerError : r))
  ]

test_mapping :: UnitTest
test_mapping =
  runTest $ embedNvimPlugin "test" [("mappingHandler", mappingHandler)] handlers do
    () <- vimCallFunction "Setup" []
    assertWait currentBufferContent (target ===)
    nvimFeedkeys "a" "x" False
    assertWait (vimGetVar "number") ((13 :: Int) ===)
