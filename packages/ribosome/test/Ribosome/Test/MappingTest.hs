module Ribosome.Test.MappingTest where

import Polysemy.Conc (interpretSync)
import qualified Polysemy.Conc.Sync as Sync
import Polysemy.Test (UnitTest, assertJust, (===))
import Polysemy.Time (Seconds (Seconds))

import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Data.Mapping (Mapping (Mapping), MappingIdent (MappingIdent))
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.Scratch (Scratch)
import Ribosome.Data.ScratchOptions (ScratchOptions (ScratchOptions))
import Ribosome.Embed (embedNvimPlugin)
import Ribosome.Host.Api.Data (nvimFeedkeys, vimCallFunction)
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Handler (rpcFunction)
import Ribosome.Scratch (showInScratch)
import Ribosome.Test.Run (rpcError, runTest)
import Ribosome.Test.Wait (assertWait)

target :: [Text]
target = ["line 1", "line 2"]

mappingHandler ::
  Members [Sync Int, Rpc !! RpcError] r =>
  Handler r ()
mappingHandler =
  void (Sync.putWait (Seconds 5) 13)

mapping :: Mapping
mapping =
  Mapping (MappingIdent "mappingHandler") "a" "n" False True

setupMappingScratch ::
  Members [Rpc !! RpcError, AtomicState (Map Text Scratch), Reader PluginName, Log, Resource] r =>
  Handler r ()
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
    rpcFunction "Setup" Sync setupMappingScratch
  ]

test_mapping :: UnitTest
test_mapping =
  runTest $ interpretSync $ embedNvimPlugin "test" [("mappingHandler", mappingHandler)] mempty handlers do
    () <- Rpc.sync (vimCallFunction @() "Setup" [])
    assertWait currentBufferContent (target ===)
    Rpc.notify (nvimFeedkeys "a" "x" False)
    assertJust (13 :: Int) =<< Sync.wait (Seconds 5)
