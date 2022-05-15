module Ribosome.Host.Test.AutocmdTest where

import Polysemy.Conc (interpretAtomic, interpretSync)
import qualified Polysemy.Conc.Sync as Sync
import Polysemy.Test (UnitTest, assertJust)
import Polysemy.Time (Seconds (Seconds))

import Ribosome.Host.Api.Effect (nvimCommand, nvimGetVar, nvimSetVar)
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Data.RpcType (fPattern)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (embedNvim)
import Ribosome.Host.Handler (rpcAutocmd)
import Ribosome.Host.Test.Run (rpcError, runTest)

var :: Text
var =
  "test_var"

au ::
  Members [Rpc !! RpcError, Sync (), Error HandlerError] r =>
  Sem r ()
au = do
  rpcError (nvimSetVar var (12 :: Int))
  void $ Sync.putWait (Seconds 5) ()

bn ::
  Members [Rpc !! RpcError, Sync (), Error HandlerError] r =>
  Sem r ()
bn = do
  rpcError (nvimSetVar var (21 :: Int))
  void $ Sync.putWait (Seconds 5) ()

regHandlers ::
  ∀ r .
  Members [AtomicState Int, Rpc !! RpcError, Sync ()] r =>
  [RpcHandler r]
regHandlers =
  [
    rpcAutocmd "User" Sync def { fPattern = "Au" } (au @(Error HandlerError : r)),
    rpcAutocmd "BufNew" Sync def (bn @(Error HandlerError : r))
  ]

test_autocmd :: UnitTest
test_autocmd =
  runTest $ interpretAtomic 0 $ interpretSync $ embedNvim regHandlers do
    nvimCommand "doautocmd User Au"
    Sync.takeWait (Seconds 5)
    assertJust @Int 12 =<< nvimGetVar var
    nvimCommand "new"
    Sync.takeWait (Seconds 5)
    assertJust @Int 21 =<< nvimGetVar var
