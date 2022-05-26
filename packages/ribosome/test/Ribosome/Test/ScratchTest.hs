module Ribosome.Test.ScratchTest where

import qualified Data.Map.Strict as Map
import Polysemy.Test (UnitTest, assertEq, unitTest)
import Test.Tasty (TestTree, testGroup)

import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Data.FloatOptions (FloatOptions (FloatOptions), FloatRelative (Cursor))
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.Scratch (Scratch)
import Ribosome.Data.ScratchOptions (ScratchOptions (ScratchOptions))
import Ribosome.Embed (embedNvimPlugin)
import Ribosome.Host.Api.Data (nvimCommand)
import Ribosome.Host.Api.Effect (vimCallFunction)
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Handler (rpcFunction)
import Ribosome.Scratch (showInScratch)
import Ribosome.Host.Test.Run (rpcError, runTest)
import Ribosome.Test.Wait (assertWait)

target :: [Text]
target = ["line 1", "line 2"]

name :: Text
name =
  "buffi"

makeScratch ::
  Members [Rpc !! RpcError, AtomicState (Map Text Scratch), Reader PluginName, Log, Resource, Stop HandlerError] r =>
  Sem r ()
makeScratch =
  rpcError (void (showInScratch target options))
  where
    options =
      ScratchOptions False True False True True True False Nothing Nothing Nothing [] [] Nothing name

floatOptions :: FloatOptions
floatOptions =
  FloatOptions Cursor 30 2 1 1 True def Nothing def False True (Just def) Nothing

makeFloatScratch ::
  Members [Rpc !! RpcError, AtomicState (Map Text Scratch), Reader PluginName, Log, Resource, Stop HandlerError] r =>
  Sem r ()
makeFloatScratch =
  rpcError (void (showInScratch target options))
  where
    options =
      ScratchOptions False True False True True True False (Just floatOptions) Nothing (Just 0) [] [] Nothing name

scratchCount ::
  Member (AtomicState (Map Text Scratch)) r =>
  Sem r Int
scratchCount =
  length . Map.toList <$> atomicGet

handlers ::
  ∀ r .
  Members [Rpc !! RpcError, AtomicState (Map Text Scratch), Reader PluginName, Log, Resource] r =>
  [RpcHandler r]
handlers =
  [
    rpcFunction "MakeFloatScratch" Sync (makeFloatScratch @(Stop HandlerError : r)),
    rpcFunction "MakeScratch" Sync (makeScratch @(Stop HandlerError : r))
  ]

scratchTest :: Text -> UnitTest
scratchTest fun = do
  runTest $ embedNvimPlugin "test" mempty mempty handlers do
    () <- vimCallFunction fun []
    assertWait scratchCount (assertEq (1 :: Int))
    assertWait currentBufferContent (assertEq target)
    Rpc.notify (nvimCommand "bdelete")
    assertWait scratchCount (assertEq 0)

test_regularScratch :: UnitTest
test_regularScratch = do
  scratchTest "MakeScratch"

test_floatScratch :: UnitTest
test_floatScratch = do
  scratchTest "MakeFloatScratch"

test_scratch :: TestTree
test_scratch =
  testGroup "scratch" [
    unitTest "regular window" test_regularScratch,
    unitTest "float window" test_floatScratch
  ]
