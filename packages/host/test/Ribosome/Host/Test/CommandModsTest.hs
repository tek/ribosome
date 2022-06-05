module Ribosome.Host.Test.CommandModsTest where

import Polysemy.Conc (interpretAtomic)
import Polysemy.Test (UnitTest, assertJust)

import Ribosome.Host.Api.Effect (nvimCommand, nvimGetVar, nvimSetVar)
import Ribosome.Host.Data.CommandMods (CommandMods (CommandMods))
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.HandlerError (HandlerError, resumeHandlerError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (embedNvim)
import Ribosome.Host.Handler (rpcCommand)
import Ribosome.Host.Unit.Run (runTest)

var :: Text
var =
  "test_var"

mods ::
  Members [Rpc !! RpcError, Stop HandlerError] r =>
  CommandMods ->
  Sem r ()
mods = \case
  CommandMods m ->
    resumeHandlerError (nvimSetVar var m)

handlers ::
  ∀ r .
  Members [AtomicState Int, Rpc !! RpcError] r =>
  [RpcHandler r]
handlers =
  [
    rpcCommand "Mods" Sync (mods @(Stop HandlerError : r))
  ]

test_mods :: UnitTest
test_mods =
  runTest $ interpretAtomic 0 $ embedNvim handlers do
    nvimCommand "belowright silent lockmarks Mods"
    assertJust @Text "belowright lockmarks silent" =<< nvimGetVar var
