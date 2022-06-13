module Main where

import Polysemy.Test (unitTest)
import Ribosome.Host.Test.ApiInfoTest (test_parseType)
import Ribosome.Host.Test.AsyncTest (test_async)
import Ribosome.Host.Test.AtomicTest (test_atomic)
import Ribosome.Host.Test.AutocmdTest (test_autocmd)
import Ribosome.Host.Test.CommandArgsTest (test_args)
import Ribosome.Host.Test.CommandBangTest (test_bang)
import Ribosome.Host.Test.CommandCompleteTest (test_complete)
import Ribosome.Host.Test.CommandModsTest (test_mods)
import Ribosome.Host.Test.CommandParamErrorTest (test_paramError)
import Ribosome.Host.Test.CommandRangeTest (test_range)
import Ribosome.Host.Test.CommandRegisterTest (test_register)
import Ribosome.Host.Test.EventTest (test_errorEvent)
import Ribosome.Host.Test.FunctionTest (test_function)
import Ribosome.Host.Test.LogTest (test_logFile)
import Ribosome.Host.Test.MaybeParamTest (test_maybeParams)
import Ribosome.Host.Test.NotifyTest (test_notify)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "host" [
    unitTest "parse api info types" test_parseType,
    unitTest "function" test_function,
    unitTest "function with Maybe params" test_maybeParams,
    unitTest "async function" test_async,
    testGroup "command" [
      unitTest "args" test_args,
      unitTest "bang" test_bang,
      unitTest "mods" test_mods,
      unitTest "range" test_range,
      unitTest "register" test_register,
      unitTest "errors" test_paramError,
      unitTest "complete" test_complete
    ],
    unitTest "autocmd" test_autocmd,
    unitTest "notify" test_notify,
    unitTest "error event" test_errorEvent,
    unitTest "atomic" test_atomic,
    unitTest "log to ile" test_logFile
  ]

main :: IO ()
main =
  defaultMain tests
