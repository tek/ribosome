module Ribosome.Host.Test.LogTest where

import Data.Generics.Labels ()
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Log (Severity (Crit))
import Path (relfile, toFilePath)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assertEq, assertLeft)

import Ribosome.Host.Api.Effect (nvimCallFunction)
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.Report (Report (Report))
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import Ribosome.Host.Embed (embedNvim)
import Ribosome.Host.Handler (rpcFunction)
import Ribosome.Host.Unit.Run (runTestConf, runUnitTest)

stopper :: Handler r ()
stopper =
  stop (Report "error" ["error", "meltdown"] Crit)

handlers :: [RpcHandler r]
handlers =
  [rpcFunction "Stopper" Sync stopper]

fileTarget :: [Text]
fileTarget =
  [
    "\ESC[35m[crit] \ESC[0m [R.H.T.LogTest#22] function:Stopper:",
    "error",
    "meltdown",
    ""
  ]

test_logFile :: UnitTest
test_logFile =
  runUnitTest do
    file <- Test.tempFile [] [relfile|log/log|]
    runTestConf (def & #hostLog . #logFile ?~ file) do
      embedNvim handlers do
        assertLeft () . first unit =<< resumeEither @RpcError @_ @_ @() (nvimCallFunction "Stopper" [])
    assertEq fileTarget . Text.lines =<< embed (Text.readFile (toFilePath file))
