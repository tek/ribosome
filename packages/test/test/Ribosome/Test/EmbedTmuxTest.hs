module Ribosome.Test.EmbedTmuxTest where

import Polysemy.Test (UnitTest, assert)

import Ribosome.Host.Api.Effect (nvimGetVar, nvimSetVar)
import Ribosome.Test.EmbedTmux (testEmbedTmux)

test_embedTmux :: UnitTest
test_embedTmux =
  testEmbedTmux do
    nvimSetVar "test" True
    assert =<< nvimGetVar "test"
