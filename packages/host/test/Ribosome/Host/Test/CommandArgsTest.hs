module Ribosome.Host.Test.CommandArgsTest where

import Conc (interpretAtomic)
import Exon (exon)
import Options.Applicative (auto, option, short, switch)
import Polysemy.Test (UnitTest, assertJust)

import Ribosome.Host.Api.Data (nvimCommand, nvimGetVar, nvimSetVar)
import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)
import Ribosome.Host.Data.Args (
  ArgList (ArgList),
  Args (Args),
  JsonArgs (JsonArgs),
  OptionParser (optionParser),
  Options (Options),
  )
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.Report (resumeReport)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Embed (embedNvim)
import Ribosome.Host.Handler (rpcCommand)
import Ribosome.Host.Unit.Run (runTest)

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
  resumeReport (nvimSetVar var a)

argList ::
  Member (Rpc !! RpcError) r =>
  Text ->
  ArgList ->
  Handler r ()
argList _ (ArgList a) =
  resumeReport (nvimSetVar var a)

jsonArgs ::
  Member (Rpc !! RpcError) r =>
  Text ->
  JsonArgs Cat ->
  Handler r ()
jsonArgs _ (JsonArgs cat) =
  resumeReport (nvimSetVar var cat)

instance OptionParser Cat where
  optionParser =
    Cat <$> option auto (short 'f') <*> switch (short 's')

options ::
  Member (Rpc !! RpcError) r =>
  Text ->
  Options Cat ->
  Handler r ()
options _ (Options cat) =
  resumeReport (nvimSetVar var cat)

handlers ::
  ∀ r .
  Members [AtomicState Int, Rpc !! RpcError] r =>
  [RpcHandler r]
handlers =
  [
    rpcCommand "Args" Sync args,
    rpcCommand "ArgList" Sync argList,
    rpcCommand "JsonArgs" Sync jsonArgs,
    rpcCommand "Options" Sync options
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
    nvimCommand [exon|Options 1 -f 15.1 -s|]
    assertJust (Cat 15.1 True) =<< nvimGetVar var
