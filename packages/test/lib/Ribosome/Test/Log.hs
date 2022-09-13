module Ribosome.Test.Log where

import Log (Severity)

import Ribosome.Test.Data.TestConfig (TestConfig)

testLogLevelConf ::
  Severity ->
  (TestConfig -> a) ->
  TestConfig ->
  a
testLogLevelConf level f conf =
  f (conf & #plugin . #host . #hostLog . #logLevelStderr .~ level)

testLogLevel ::
  Severity ->
  (TestConfig -> a) ->
  a
testLogLevel level f =
  testLogLevelConf level f def
