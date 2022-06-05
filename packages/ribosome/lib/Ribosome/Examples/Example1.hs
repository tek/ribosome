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
  runNvimPluginIO_ (PluginConfig "counter" def) [rpcFunction "Count" Sync count] mempty mempty
