module Ribosome.Host.Test.CommandCompleteTest where

import Conc (interpretSync)
import qualified Data.Text as Text
import Exon (exon)
import Polysemy.Test (UnitTest, assertEq, (===))
import qualified Sync
import Time (Seconds (Seconds))

import Ribosome.Host.Api.Effect (nvimCallFunction, nvimGetVar, nvimInput, nvimSetVar)
import Ribosome.Host.Class.Msgpack.Array (msgpackArray)
import Ribosome.Host.Data.Args (Args (Args))
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.Report (resumeReport)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import Ribosome.Host.Data.RpcType (CompleteStyle (CompleteFiltered, CompleteUnfiltered))
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (embedNvim)
import Ribosome.Host.Handler (completeWith, rpcCommand)
import Ribosome.Host.Unit.Run (runTest)

var :: Text
var =
  "var"

completing ::
  Members [Sync (), Rpc !! RpcError] r =>
  Args ->
  Handler r ()
completing (Args a) = do
  resumeReport (nvimSetVar var a)
  void (Sync.putWait (Seconds 5) ())

dictionary :: [Text]
dictionary =
  [
    "completion",
    "somethingelse"
  ]

completeFiltered :: Text -> Text -> Int -> Handler r [Text]
completeFiltered lead _ _ =
  pure (filter (Text.isPrefixOf lead) dictionary)

completeUnfiltered :: Text -> Text -> Int -> Handler r [Text]
completeUnfiltered _ _ _ =
  pure dictionary

handlers ::
  ∀ r .
  Members [Sync (), Rpc !! RpcError] r =>
  [RpcHandler r]
handlers =
  completeWith CompleteFiltered completeFiltered (rpcCommand "CompletingFiltered" Sync completing)
  <>
  completeWith CompleteUnfiltered completeUnfiltered (rpcCommand "CompletingUnfiltered" Sync completing)

test_complete :: UnitTest
test_complete =
  runTest $ interpretSync $ embedNvim handlers do
    test "CompletingFiltered"
    nvimSetVar var ("reset" :: Text)
    test "CompletingUnfiltered"
    res <- nvimCallFunction "Complete_CompletingUnfiltered" (msgpackArray ("comp" :: Text) ("" :: Text) (0 :: Int))
    Text.unlines dictionary === res
  where
    test fun = do
      void (nvimInput [exon|:#{fun} comp<tab><cr>|])
      Sync.takeWait (Seconds 5)
      assertEq "completion" =<< nvimGetVar @Text var
