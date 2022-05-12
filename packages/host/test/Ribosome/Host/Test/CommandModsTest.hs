module Ribosome.Host.Test.CommandModsTest where

import Polysemy.Conc (interpretAtomic, interpretSync)
import Polysemy.Test (UnitTest, assertJust)

import Ribosome.Host.Api.Effect (nvimCommand, nvimGetVar, nvimSetVar)
import Ribosome.Host.Data.CommandMods
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Handler (rpcCommand)
import Ribosome.Host.Test.Run (embedNvim, rpcError, runTest)

var :: Text
var =
  "test_var"

mods ::
  Members [Rpc !! RpcError, Error HandlerError] r =>
  CommandMods ->
  Sem r ()
mods = \case
  CommandMods m ->
    rpcError (nvimSetVar var m)

modsHandlers ::
  ∀ r .
  Members [AtomicState Int, Rpc !! RpcError] r =>
  [RpcHandler r]
modsHandlers =
  [
    rpcCommand "Mods" Sync (mods @(Error HandlerError : r))
  ]

test_mods :: UnitTest
test_mods =
  runTest $ interpretAtomic 0 $ embedNvim modsHandlers $ interpretSync do
    nvimCommand "belowright silent lockmarks Mods"
    assertJust @Text "belowright lockmarks silent" =<< nvimGetVar var
