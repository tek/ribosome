module Main where

import Polysemy.Test (unitTest)
import Ribosome.Host.Test.ApiInfoTest (test_parseType)
import Ribosome.Host.Test.AtomicTest (test_atomic)
import Ribosome.Host.Test.AutocmdTest (test_autocmd)
import Ribosome.Host.Test.CommandArgsTest (test_args)
import Ribosome.Host.Test.CommandBangTest (test_bang)
import Ribosome.Host.Test.CommandModsTest (test_mods)
import Ribosome.Host.Test.CommandParamErrorTest (test_paramError)
import Ribosome.Host.Test.CommandRangeTest (test_range)
import Ribosome.Host.Test.CommandRegisterTest (test_register)
import Ribosome.Host.Test.EventTest (test_errorEvent)
import Ribosome.Host.Test.FunctionTest (test_function)
import Ribosome.Host.Test.NotifyTest (test_notify)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "host" [
    unitTest "parse api info types" test_parseType,
    unitTest "function" test_function,
    testGroup "command" [
      unitTest "args" test_args,
      unitTest "bang" test_bang,
      unitTest "mods" test_mods,
      unitTest "range" test_range,
      unitTest "register" test_register,
      unitTest "errors" test_paramError
    ],
    unitTest "autocmd" test_autocmd,
    unitTest "notify" test_notify,
    unitTest "error event" test_errorEvent,
    unitTest "atomic" test_atomic
  ]

main :: IO ()
main =
  defaultMain tests
