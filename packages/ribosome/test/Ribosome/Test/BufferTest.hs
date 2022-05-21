module Ribosome.Test.BufferTest where

import Path (relfile)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assert, assertEq, evalMaybe, (/==), (===))

import Ribosome.Api.Buffer (bufferCount, bufferForFile, bufferIsFile, edit)
import Ribosome.Data.FileBuffer (FileBuffer (FileBuffer))
import Ribosome.Host.Api.Effect (nvimGetCurrentBuf)
import Ribosome.Test.Run (embedTest_)

test_bufferForFile :: UnitTest
test_bufferForFile =
  embedTest_ do
    file1 <- Test.tempFile [] [relfile|file.x|]
    file2 <- Test.tempFile [] [relfile|file.y|]
    edit file1
    edit file2
    assertEq 2 =<< bufferCount
    cb <- nvimGetCurrentBuf
    assert =<< bufferIsFile cb
    FileBuffer buf path <- evalMaybe =<< bufferForFile file1
    cb /== buf
    path === file1
