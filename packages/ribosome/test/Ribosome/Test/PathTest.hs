module Ribosome.Test.PathTest where

import Path (reldir)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assertEq)

import Ribosome.Api.Path (nvimDir, nvimSetCwd)
import Ribosome.Host.Test.Run (embedTest_)
import Ribosome.Test.Error (testHandler)

test_nvimPath :: UnitTest
test_nvimPath =
  embedTest_ $ testHandler do
    dir <- Test.tempDir [reldir|path|]
    nvimSetCwd dir
    assertEq dir =<< nvimDir ""
