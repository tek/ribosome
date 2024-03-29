module Ribosome.Host.Test.CommandModsTest where

import Conc (interpretAtomic)
import Polysemy.Test (UnitTest, assertJust)

import Ribosome.Host.Api.Data (nvimCommand, nvimGetVar, nvimSetVar)
import Ribosome.Host.Data.CommandMods (CommandMods (CommandMods))
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.Report (resumeReport)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (embedNvim)
import Ribosome.Host.Handler (rpcCommand)
import Ribosome.Host.Unit.Run (runTest)

var :: Text
var =
  "test_var"

mods ::
  Member (Rpc !! RpcError) r =>
  CommandMods ->
  Handler r ()
mods = \case
  CommandMods m ->
    resumeReport (nvimSetVar var m)

handlers ::
  ∀ r .
  Members [AtomicState Int, Rpc !! RpcError] r =>
  [RpcHandler r]
handlers =
  [
    rpcCommand "Mods" Sync mods
  ]

test_mods :: UnitTest
test_mods =
  runTest $ interpretAtomic 0 $ embedNvim handlers do
    nvimCommand "belowright silent lockmarks Mods"
    assertJust @Text "lockmarks silent belowright" =<< nvimGetVar var
