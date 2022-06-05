module Ribosome.Host.Test.CommandRangeTest where

import Polysemy.Test (UnitTest, assertJust)

import Ribosome.Host.Api.Effect (
  nvimBufSetLines,
  nvimCommand,
  nvimGetCurrentBuf,
  nvimGetCurrentWin,
  nvimGetVar,
  nvimSetVar,
  nvimWinSetCursor,
  )
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.HandlerError (resumeHandlerError)
import Ribosome.Host.Data.Range (Range (Range), RangeStyle (RangeCount, RangeFile, RangeLine))
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (embedNvim)
import Ribosome.Host.Handler (rpcCommand)
import Ribosome.Host.Unit.Run (runTest)

var :: Text
var =
  "test_var"

rangeFile ::
  Member (Rpc !! RpcError) r =>
  Range 'RangeFile ->
  Int64 ->
  Handler r ()
rangeFile = \case
  Range l (Just h) ->
    \ i -> resumeHandlerError (nvimSetVar var (l, h, i))
  Range _ Nothing ->
    const (stop "no upper range bound given")

rangeLine ::
  Member (Rpc !! RpcError) r =>
  Range ('RangeLine 'Nothing) ->
  Handler r ()
rangeLine = \case
  Range l (Just h) ->
    resumeHandlerError (nvimSetVar var (l, h))
  Range _ Nothing ->
    stop "no upper range bound given"

rangeLineDefault ::
  Member (Rpc !! RpcError) r =>
  Range ('RangeLine ('Just 13)) ->
  Handler r ()
rangeLineDefault = \case
  Range l Nothing ->
    resumeHandlerError (nvimSetVar var l)
  Range _ (Just _) ->
    stop "range line count function got upper bound"

rangeCountImplicit ::
  Member (Rpc !! RpcError) r =>
  Range ('RangeCount 'Nothing) ->
  Handler r ()
rangeCountImplicit = \case
  Range _ (Just _) ->
    stop "range count function got upper bound"
  Range l Nothing ->
    resumeHandlerError (nvimSetVar var l)

rangeCountDefault ::
  Member (Rpc !! RpcError) r =>
  Range ('RangeCount ('Just 23)) ->
  Handler r ()
rangeCountDefault = \case
  Range _ (Just _) ->
    stop "range count function got upper bound"
  Range l Nothing ->
    resumeHandlerError (nvimSetVar var l)

rangeHandlers ::
  ∀ r .
  Member (Rpc !! RpcError) r =>
  [RpcHandler r]
rangeHandlers =
  [
    rpcCommand "RangeFile" Sync rangeFile,
    rpcCommand "RangeLine" Sync rangeLine,
    rpcCommand "RangeLineDefault" Sync rangeLineDefault,
    rpcCommand "RangeCountImplicit" Sync rangeCountImplicit,
    rpcCommand "RangeCountDefault" Sync rangeCountDefault
  ]

test_range :: UnitTest
test_range =
  runTest $ embedNvim rangeHandlers do
    buf <- nvimGetCurrentBuf
    win <- nvimGetCurrentWin
    nvimBufSetLines buf 0 1 True ["1", "2", "3", "4", "5"]
    nvimWinSetCursor win (3, 1)
    nvimCommand "RangeFile 9"
    assertJust @(Int64, Int64, Int64) (1, 5, 9) =<< nvimGetVar var
    nvimCommand "RangeLine"
    assertJust @(Int64, Int64) (3, 3) =<< nvimGetVar var
    nvimCommand "2,4RangeLine"
    assertJust @(Int64, Int64) (2, 4) =<< nvimGetVar var
    nvimCommand "RangeLineDefault"
    assertJust @Int64 13 =<< nvimGetVar var
    nvimCommand "RangeCountImplicit"
    assertJust @Int64 0 =<< nvimGetVar var
    nvimCommand "RangeCountDefault"
    assertJust @Int64 23 =<< nvimGetVar var
    nvimCommand "144RangeCountDefault"
    assertJust @Int64 144 =<< nvimGetVar var
    nvimCommand "RangeCountDefault 201"
    assertJust @Int64 201 =<< nvimGetVar var
