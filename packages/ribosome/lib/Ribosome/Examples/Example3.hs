-- |An example for the docs.
module Ribosome.Examples.Example3 where

import qualified Data.Text.IO as Text

import Ribosome
import Ribosome.Api

ping :: Handler r Text
ping =
  pure "Ping"

main :: IO ()
main =
  runEmbedPluginIO_ "ping-plugin" [rpcFunction "Ping" Sync ping] do
    ignoreRpcError do
      embed . Text.putStrLn =<< nvimCallFunction "Ping" []
