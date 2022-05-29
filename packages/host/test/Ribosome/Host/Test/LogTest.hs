module Ribosome.Host.Test.LogTest where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Log (Severity (Crit))
import Path (relfile, toFilePath)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assertEq, assertLeft)

import Ribosome.Host.Api.Effect (nvimCallFunction)
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.HandlerError (ErrorMessage (ErrorMessage), handlerError)
import Ribosome.Host.Data.HostConfig (HostConfig (log), LogConfig (logFile))
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcHandler (Handler, RpcHandler)
import Ribosome.Host.Embed (embedNvimConf)
import Ribosome.Host.Handler (rpcFunction)
import Ribosome.Host.Interpreter.Handlers (interpretHandlers)
import Ribosome.Host.Test.Run (runTest)

stopper :: Handler r ()
stopper =
  handlerError (ErrorMessage "error" ["error", "meltdown"] Crit) "test"

handlers :: [RpcHandler r]
handlers =
  [rpcFunction "Stopper" Sync stopper]

fileTarget :: [Text]
fileTarget =
  [
    "\ESC[35m[crit] \ESC[0m [R.H.T.LogTest#23] error",
    "meltdown",
    ""
  ]

test_logFile :: UnitTest
test_logFile =
  runTest do
    file <- Test.tempFile [] [relfile|log/log|] 
    embedNvimConf def { log = def { logFile = Just file } } (interpretHandlers handlers) do
      assertLeft () . first unit =<< resumeEither @RpcError @_ @_ @() (nvimCallFunction "Stopper" [])
    assertEq fileTarget . Text.lines =<< embed (Text.readFile (toFilePath file))
