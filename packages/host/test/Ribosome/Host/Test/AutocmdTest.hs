module Ribosome.Host.Test.AutocmdTest where

import Polysemy.Conc (interpretSync)
import qualified Polysemy.Conc.Sync as Sync
import Polysemy.Test (UnitTest, assertJust)
import Polysemy.Time (Seconds (Seconds))

import Ribosome.Host.Api.Effect (nvimCommand, nvimGetVar, nvimSetVar)
import Ribosome.Host.Data.Execution (Execution (Async))
import Ribosome.Host.Data.HandlerError (HandlerError, resumeHandlerError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Data.RpcType (fPattern)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (embedNvim)
import Ribosome.Host.Handler (rpcAutocmd)
import Ribosome.Host.Unit.Run (runTest)

var :: Text
var =
  "test_var"

au ::
  Members [Rpc !! RpcError, Sync (), Stop HandlerError] r =>
  Sem r ()
au = do
  resumeHandlerError (nvimSetVar var (12 :: Int))
  void $ Sync.putWait (Seconds 5) ()

bn ::
  Members [Rpc !! RpcError, Sync (), Stop HandlerError] r =>
  Sem r ()
bn = do
  resumeHandlerError (nvimSetVar var (21 :: Int))
  void $ Sync.putWait (Seconds 5) ()

handlers ::
  ∀ r .
  Members [Rpc !! RpcError, Sync ()] r =>
  [RpcHandler r]
handlers =
  [
    rpcAutocmd "Au" Async "User" def { fPattern = "Au" } (au @(Stop HandlerError : r)),
    rpcAutocmd "Bn" Async "BufNew" def (bn @(Stop HandlerError : r))
  ]

test_autocmd :: UnitTest
test_autocmd =
  runTest $ interpretSync $ embedNvim handlers do
    nvimCommand "doautocmd User Au"
    Sync.takeWait (Seconds 5)
    assertJust @Int 12 =<< nvimGetVar var
    nvimCommand "new"
    Sync.takeWait (Seconds 5)
    assertJust @Int 21 =<< nvimGetVar var
