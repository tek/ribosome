module Plugin.Test.PingTest where

import Polysemy.Test (UnitTest, (===))
import Ribosome.Api (nvimCallFunction)
import Ribosome.Test (embedPluginTestWith)

import Plugin (PluginStack, handlers, interpretPluginStack)

test_ping :: UnitTest
test_ping =
  embedPluginTestWith @PluginStack handlers interpretPluginStack do
    r <- call *> call *> call
    (3 :: Int) === r
  where
    call =
      nvimCallFunction "PluginPing" []
