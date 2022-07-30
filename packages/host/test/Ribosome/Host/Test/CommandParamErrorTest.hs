module Ribosome.Host.Test.CommandParamErrorTest where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import qualified Data.Text as Text
import Polysemy.Test (Hedgehog, UnitTest, assertLeft, runTestAuto)

import Ribosome.Host.Test.CommandParamErrorDecls (argAfterArgs, argsAfterArg)
import Data.MessagePack (Object)

typeError ::
  Members [Hedgehog IO, Embed IO] r =>
  [Text] ->
  (Map Text Object, [Text]) ->
  Sem r ()
typeError msg t = do
  e <- tryAny (evaluate (force t))
  assertLeft msg (first trunc e)
  where
    trunc =
      fmap Text.strip . take (length msg) . drop 1 . lines

argAfterArgsError :: [Text]
argAfterArgsError =
  [
    "• Custom parameter types (here Int) cannot be combined with Args",
    "since Args consumes all arguments"
  ]

argsAfterArgError :: [Text]
argsAfterArgError =
  [
    "• Command option type Bang may not come after non-option"
  ]

test_paramError :: UnitTest
test_paramError =
  runTestAuto do
    typeError argAfterArgsError argAfterArgs
    typeError argsAfterArgError argsAfterArg
