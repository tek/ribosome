module Ribosome.Test.VariableTest where

import Polysemy.Test (UnitTest, assertEq, assertRight, unitTest)
import Test.Tasty (TestTree, testGroup)

import Ribosome.Api (nvimGetCurrentTabpage)
import Ribosome.Api.Variable (
  VariableError (..),
  VariableName (VariableName),
  VariableResult (..),
  VariableScope (..),
  allScopes,
  findVariable,
  variableValues,
  )
import Ribosome.Host.Api.Data (
  nvimBufSetVar,
  nvimGetCurrentBuf,
  nvimGetCurrentWin,
  nvimSetVar,
  nvimTabpageSetVar,
  nvimWinSetVar,
  )
import Ribosome.Host.Test.Run (embedTest_)

name :: Text
name = "test_var"

targetValues :: Map VariableScope (VariableResult Int)
targetValues =
  [
    (GlobalScope, VariableDefined 5),
    (TabpageScope, VariableError VariableUndefined),
    (WindowScope, VariableDefined 9),
    (BufferScope, VariableError VariableTypeMismatch)
  ]

test_variableValues :: UnitTest
test_variableValues =
  embedTest_ do
    win <- nvimGetCurrentWin
    buf <- nvimGetCurrentBuf
    nvimSetVar @Int name 5
    nvimWinSetVar @Int win name 9
    nvimBufSetVar @Text buf name "text"
    assertEq targetValues =<< variableValues (VariableName name) allScopes

test_findVariable :: UnitTest
test_findVariable =
  embedTest_ do
    tab <- nvimGetCurrentTabpage
    win <- nvimGetCurrentWin
    buf <- nvimGetCurrentBuf
    nvimTabpageSetVar @Text tab name "text"
    nvimWinSetVar @Int win name 14
    nvimBufSetVar @Int buf name 41
    assertRight (14 :: Int) =<< findVariable (VariableName name) allScopes

test_variable :: TestTree
test_variable =
  testGroup "variable" [
    unitTest "lookup in all scopes" test_variableValues,
    unitTest "find the first scope" test_findVariable
  ]
