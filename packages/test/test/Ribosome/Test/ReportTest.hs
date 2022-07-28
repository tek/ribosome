module Ribosome.Test.ReportTest where

import qualified Log
import Polysemy.Test (UnitTest)

import Ribosome.Host.Api.Effect (nvimCommand)
import Ribosome.Test.Screenshot (awaitScreenshot)
import Ribosome.Test.SocketTmux (testSocketTmux)
import qualified Ribosome.Report as Report

test_report :: UnitTest
test_report =
  testSocketTmux do
    nvimCommand "mode"
    Log.debug "don't report Log.debug message"
    awaitScreenshot False "report-1" 0
    Log.info "report Log.info message"
    awaitScreenshot False "report-2" 0
    Report.warn "report Report.warn message" []
    awaitScreenshot False "report-3" 0
