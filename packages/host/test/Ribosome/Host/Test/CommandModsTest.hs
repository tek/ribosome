module Ribosome.Host.Test.CommandModsTest where

import Polysemy.Conc (interpretAtomic)
import Polysemy.Test (UnitTest, assertJust)

import Ribosome.Host.Api.Effect (nvimCommand, nvimGetVar, nvimSetVar)
import Ribosome.Host.Data.CommandMods (CommandMods (CommandMods))
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (embedNvim)
import Ribosome.Host.Handler (rpcCommand)
import Ribosome.Host.Test.Run (rpcError, runTest)

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
  runTest $ interpretAtomic 0 $ embedNvim modsHandlers do
    nvimCommand "belowright silent lockmarks Mods"
    assertJust @Text "belowright lockmarks silent" =<< nvimGetVar var
