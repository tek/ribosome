{-# options_haddock prune, hide #-}

-- |An example for the docs.
module Ribosome.Examples.Example1 where

import Ribosome
import Ribosome.Api

count ::
  Member (Rpc !! RpcError) r =>
  Int ->
  Handler r Int
count n = do
  s <- 0 <! nvimGetVar "counter"
  let s' = s + n
  ignoreRpcError (nvimSetVar "counter" s')
  pure s'

main :: IO ()
main =
  runNvimPluginIO_ "counter" [rpcFunction "Count" Sync count]
