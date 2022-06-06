module Ribosome.Test.TmuxTest where

import Polysemy.Test (UnitTest, assert)

import Ribosome.Host.Api.Effect (nvimGetVar, nvimSetVar)
import Ribosome.Test.Tmux (testEmbedTmux)

test_tmux :: UnitTest
test_tmux =
  testEmbedTmux do
    nvimSetVar "test" True
    assert =<< nvimGetVar "test"
