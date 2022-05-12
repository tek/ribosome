module Ribosome.Host.Test.CommandRangeTest where

import Polysemy.Conc (interpretAtomic, interpretSync)
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
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.Range (Range (Range), RangeStyle (RangeCount, RangeFile, RangeLine))
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Handler (rpcCommand)
import Ribosome.Host.Test.Run (embedNvim, rpcError, runTest)

var :: Text
var =
  "test_var"

rangeFile ::
  Members [Rpc !! RpcError, Error HandlerError] r =>
  Range 'RangeFile ->
  Int64 ->
  Sem r ()
rangeFile = \case
  Range l (Just h) ->
    \ i -> rpcError (nvimSetVar var (l, h, i))
  Range _ Nothing ->
    const (throw "no upper range bound given")

rangeLine ::
  Members [Rpc !! RpcError, Error HandlerError] r =>
  Range ('RangeLine 'Nothing) ->
  Sem r ()
rangeLine = \case
  Range l (Just h) ->
    rpcError (nvimSetVar var (l, h))
  Range _ Nothing ->
    throw "no upper range bound given"

rangeLineDefault ::
  Members [Rpc !! RpcError, Error HandlerError] r =>
  Range ('RangeLine ('Just 13)) ->
  Sem r ()
rangeLineDefault = \case
  Range l Nothing ->
    rpcError (nvimSetVar var l)
  Range _ (Just _) ->
    throw "range line count function got upper bound"

rangeCountImplicit ::
  Members [Rpc !! RpcError, Error HandlerError] r =>
  Range ('RangeCount 'Nothing) ->
  Sem r ()
rangeCountImplicit = \case
  Range _ (Just _) ->
    throw "range count function got upper bound"
  Range l Nothing ->
    rpcError (nvimSetVar var l)

rangeCountDefault ::
  Members [Rpc !! RpcError, Error HandlerError] r =>
  Range ('RangeCount ('Just 23)) ->
  Sem r ()
rangeCountDefault = \case
  Range _ (Just _) ->
    throw "range count function got upper bound"
  Range l Nothing ->
    rpcError (nvimSetVar var l)

rangeHandlers ::
  ∀ r .
  Members [AtomicState Int, Rpc !! RpcError] r =>
  [RpcHandler r]
rangeHandlers =
  [
    rpcCommand "RangeFile" Sync (rangeFile @(Error HandlerError : r)),
    rpcCommand "RangeLine" Sync (rangeLine @(Error HandlerError : r)),
    rpcCommand "RangeLineDefault" Sync (rangeLineDefault @(Error HandlerError : r)),
    rpcCommand "RangeCountImplicit" Sync (rangeCountImplicit @(Error HandlerError : r)),
    rpcCommand "RangeCountDefault" Sync (rangeCountDefault @(Error HandlerError : r))
  ]

test_range :: UnitTest
test_range =
  runTest $ interpretAtomic 0 $ embedNvim rangeHandlers $ interpretSync do
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
