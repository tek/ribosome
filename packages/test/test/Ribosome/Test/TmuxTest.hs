module Ribosome.Test.TmuxTest where

import Polysemy.Test (UnitTest, assert)

import Ribosome.Host.Api.Effect (nvimGetVar, nvimSetVar)
import Ribosome.Test.Tmux (testRibosomeTmux)

test_tmux :: UnitTest
test_tmux =
  testRibosomeTmux do
    nvimSetVar "test" True
    assert =<< nvimGetVar "test"
