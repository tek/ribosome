module Ribosome.Test.SocketTmuxTest where

import Polysemy.Test (UnitTest, assert)

import Ribosome.Host.Api.Data (nvimGetVar, nvimSetVar)
import Ribosome.Test.SocketTmux (testSocketTmux)

test_socketTmux :: UnitTest
test_socketTmux =
  testSocketTmux do
    nvimSetVar "test" True
    assert =<< nvimGetVar "test"
