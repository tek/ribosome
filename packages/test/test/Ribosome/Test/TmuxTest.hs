module Ribosome.Test.TmuxTest where

import Polysemy.Test (UnitTest, assert)

import Ribosome.Host.Api.Effect (nvimGetVar, nvimSetVar)
import Ribosome.Test.Tmux (tmuxPluginTestConf)

test_tmux :: UnitTest
test_tmux =
  tmuxPluginTestConf def mempty mempty mempty do
    nvimSetVar "test" True
    assert =<< nvimGetVar "test"
