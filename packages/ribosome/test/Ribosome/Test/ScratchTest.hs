module Ribosome.Test.ScratchTest where

import Polysemy.Test (UnitTest, assertEq, unitTest)
import Test.Tasty (TestTree, testGroup)

import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Data.FloatOptions (FloatOptions (FloatOptions), FloatRelative (Cursor))
import Ribosome.Data.ScratchId (ScratchId)
import Ribosome.Data.ScratchOptions (ScratchOptions (ScratchOptions))
import qualified Ribosome.Effect.Scratch as Scratch
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Host.Api.Data (nvimCommand)
import Ribosome.Host.Api.Effect (vimCallFunction)
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.HandlerError (resumeHandlerError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Handler (rpcFunction)
import Ribosome.Test.Wait (assertWait)
import Ribosome.Unit.Run (runTestHandlers)

target :: [Text]
target = ["line 1", "line 2"]

name :: ScratchId
name =
  "buffi"

makeScratch ::
  Member (Scratch !! RpcError) r =>
  Handler r ()
makeScratch =
  resumeHandlerError (void (Scratch.show target options))
  where
    options =
      ScratchOptions False True False True True True False Nothing Nothing Nothing [] [] Nothing name

floatOptions :: FloatOptions
floatOptions =
  FloatOptions Cursor 30 2 1 1 True def Nothing def False True (Just def) Nothing

makeFloatScratch ::
  Member (Scratch !! RpcError) r =>
  Handler r ()
makeFloatScratch =
  resumeHandlerError (void (Scratch.show target options))
  where
    options =
      ScratchOptions False True False True True True False (Just floatOptions) Nothing (Just 0) [] [] Nothing name

scratchCount ::
  Member (Scratch !! RpcError) r =>
  Sem r Int
scratchCount =
  (-1) <! (length <$> Scratch.get)

handlers ::
  ∀ r .
  Member (Scratch !! RpcError) r =>
  [RpcHandler r]
handlers =
  [
    rpcFunction "MakeFloatScratch" Sync makeFloatScratch,
    rpcFunction "MakeScratch" Sync makeScratch
  ]

scratchTest :: Text -> UnitTest
scratchTest fun = do
  runTestHandlers handlers mempty do
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
