module Ribosome.Test.ReportTest where

import qualified Log
import Log (Severity (Error, Info))
import Polysemy.Test (UnitTest)

import Ribosome.Host.Api.Data (nvimCommand, nvimInput)
import Ribosome.Host.Data.Execution (Execution (Sync))
import Ribosome.Host.Data.Report (Report (Report))
import Ribosome.Host.Data.RpcHandler (Handler)
import Ribosome.Host.Handler (rpcFunction)
import qualified Ribosome.Report as Report
import Ribosome.Test.Screenshot (awaitScreenshot)
import Ribosome.Test.SocketTmux (testHandlersSocketTmux)

stopInfo :: Handler r ()
stopInfo =
  stop (Report "report an info Stop by echoing" [] Info)

stopError :: Handler r ()
stopError =
  stop (Report "report an error Stop by responding" [] Error)

test_report :: UnitTest
test_report =
  testHandlersSocketTmux [rpcFunction "StopInfo" Sync stopInfo, rpcFunction "StopError" Sync stopError] do
    nvimCommand "mode"
    Log.debug "don't report Log.debug message"
    awaitScreenshot False "report-1" 0
    Log.info "report Log.info message"
    awaitScreenshot False "report-2" 0
    Report.warn "report Report.warn message" []
    awaitScreenshot False "report-3" 0
    nvimInput ":call StopInfo()<cr>"
    awaitScreenshot False "report-4" 0
    nvimInput ":call StopError()<cr>"
    awaitScreenshot False "report-5" 0
