module Ribosome.Test.Log where

import Log (Severity)

import Ribosome.Test.Data.TestConfig (TestConfig)

testLogLevelConf ::
  HasCallStack =>
  Severity ->
  (TestConfig -> a) ->
  TestConfig ->
  a
testLogLevelConf =
  undefined

testLogLevel ::
  HasCallStack =>
  Severity ->
  (TestConfig -> a) ->
  a
testLogLevel level f =
  testLogLevelConf level f def
