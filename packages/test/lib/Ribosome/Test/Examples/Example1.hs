{-# options_haddock prune, hide #-}

-- |An example for the docs.
module Ribosome.Test.Examples.Example1 where

import Polysemy.Test

import Ribosome
import Ribosome.Api
import Ribosome.Test

store ::
  Member (Rpc !! RpcError) r =>
  Args ->
  Handler r ()
store (Args msg) =
  ignoreRpcError do
    nvimSetVar "message" msg

test_direct :: UnitTest
test_direct =
  testEmbed_ do
    store "test directly"
    assertEq "test directly" =<< nvimGetVar @Text "message"

test_rpc :: UnitTest
test_rpc =
  testPlugin_ [rpcCommand "Store" Sync store] do
    nvimCommand "Store test RPC"
    assertEq "test RPC" =<< nvimGetVar @Text "message"
