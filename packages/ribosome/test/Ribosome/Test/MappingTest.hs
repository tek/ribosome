module Ribosome.Test.MappingTest where

import Conc (interpretSync)
import qualified Polysemy.Conc.Sync as Sync
import Polysemy.Test (UnitTest, assertJust, (===))
import Polysemy.Time (Seconds (Seconds))

import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Data.Mapping (MapMode (MapNormal), Mapping)
import Ribosome.Data.ScratchOptions (ScratchOptions (ScratchOptions))
import qualified Ribosome.Effect.Scratch as Scratch
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Host.Api.Data (nvimFeedkeys, vimCallFunction)
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.Report (resumeReport)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Handler (rpcCommand, rpcFunction)
import Ribosome.Mapping (mappingFor)
import Ribosome.Test.Wait (assertWait)
import Ribosome.Unit.Run (runTest, testHandlers)

target :: [Text]
target = ["line 1", "line 2"]

mappingHandler ::
  Members [Sync Int, Rpc !! RpcError] r =>
  Handler r ()
mappingHandler =
  void (Sync.putWait (Seconds 5) 13)

setupMappingScratch ::
  Member (Scratch !! RpcError) r =>
  Mapping ->
  Handler r ()
setupMappingScratch mapping = do
  resumeReport (void (Scratch.show target options))
  where
    options =
      ScratchOptions False True False True True True False Nothing Nothing Nothing [] [mapping] Nothing "mappo"

handlers ::
  ∀ r .
  Members [Sync Int, Scratch !! RpcError, Rpc !! RpcError] r =>
  [RpcHandler r]
handlers =
  [
    rpcFunction "Setup" Sync (setupMappingScratch m),
    mapRpc
  ]
  where
    m =
      mappingFor mapRpc "a" [MapNormal] Nothing mempty
    mapRpc =
      rpcCommand "Map" Sync mappingHandler

test_mapping :: UnitTest
test_mapping =
  runTest $ interpretSync $ testHandlers handlers mempty do
    () <- Rpc.sync (vimCallFunction @() "Setup" [])
    assertWait currentBufferContent (target ===)
    Rpc.notify (nvimFeedkeys "a" "x" False)
    assertJust (13 :: Int) =<< Sync.wait (Seconds 5)
