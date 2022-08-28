module Ribosome.App.Templates.PingTestHs where

import Exon (exon)

import Ribosome.App.Data (ModuleName (ModuleName))

pingTestHs :: ModuleName -> Text
pingTestHs (ModuleName modName) =
  [exon|module #{modName}.Test.PingTest where

import Polysemy.Test (UnitTest, (===))
import Ribosome.Api (nvimCallFunction)
import Ribosome.Test (testPlugin)

import #{modName}.Plugin (#{modName}Stack, handlers, interpret#{modName}Stack)

test_ping :: UnitTest
test_ping =
  testPlugin @#{modName}Stack interpret#{modName}Stack handlers do
    r <- call *> call *> call
    (3 :: Int) === r
  where
    call =
      nvimCallFunction "#{modName}Ping" []
|]
