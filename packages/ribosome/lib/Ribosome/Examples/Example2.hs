-- |An example for the docs.
module Ribosome.Examples.Example2 where

import Data.MessagePack (Object)

import Ribosome
import Ribosome.Api

changed ::
  Members NvimPlugin r =>
  Object ->
  Handler r ()
changed value =
  ignoreRpcError (echo ("Update value to: " <> show value))

main :: IO ()
main =
  runRemoteStack "watch-plugin" (watchVariables [("trigger", changed)] remotePlugin)
