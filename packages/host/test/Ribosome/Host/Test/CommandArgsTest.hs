module Ribosome.Host.Test.CommandArgsTest where

import Conc (interpretAtomic)
import Exon (exon)
import Polysemy.Test (UnitTest, assertJust)

import Ribosome.Host.Api.Effect (nvimCommand, nvimGetVar, nvimSetVar)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)
import Ribosome.Host.Data.Args (ArgList (ArgList), Args (Args), JsonArgs (JsonArgs))
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.HandlerError (resumeHandlerError)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (embedNvim)
import Ribosome.Host.Handler (rpcCommand)
import Ribosome.Host.Unit.Run (runTest)
import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)

data Cat =
  Cat {
    fuzziness :: Double,
    sleepy :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, MsgpackEncode, MsgpackDecode)

var :: Text
var =
  "test_var"

args ::
  Member (Rpc !! RpcError) r =>
  Args ->
  Handler r ()
args (Args a) =
  resumeHandlerError (nvimSetVar var a)

argList ::
  Member (Rpc !! RpcError) r =>
  Text ->
  ArgList ->
  Handler r ()
argList _ (ArgList a) =
  resumeHandlerError (nvimSetVar var a)

jsonArgs ::
  Member (Rpc !! RpcError) r =>
  Text ->
  JsonArgs Cat ->
  Handler r ()
jsonArgs _ (JsonArgs cat) =
  resumeHandlerError (nvimSetVar var cat)

handlers ::
  ∀ r .
  Members [AtomicState Int, Rpc !! RpcError] r =>
  [RpcHandler r]
handlers =
  [
    rpcCommand "Args" Sync args,
    rpcCommand "ArgList" Sync argList,
    rpcCommand "JsonArgs" Sync jsonArgs
  ]

test_args :: UnitTest
test_args =
  runTest $ interpretAtomic 0 $ embedNvim handlers do
    nvimCommand "Args 1 2 3 4 5"
    assertJust @Text "1 2 3 4 5" =<< nvimGetVar var
    nvimCommand "ArgList 1 2 3 4 5"
    assertJust @[Text] ["2", "3", "4", "5"] =<< nvimGetVar var
    nvimCommand [exon|JsonArgs 1 { "fuzziness": 15.1, "sleepy": true }|]
    assertJust (Cat 15.1 True) =<< nvimGetVar var
