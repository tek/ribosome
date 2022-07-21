module Ribosome.Test.EmbedTmux where

import Polysemy.Test (UnitTest)

import Ribosome.Embed (HandlerEffects, interpretPluginEmbed)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Interpreter.Handlers (withHandlers)
import Ribosome.Test.Data.TestConfig (TmuxTestConfig)
import Ribosome.Test.Embed (TestEffects, testPluginEmbed)
import Ribosome.Test.TmuxCommon (TmuxStack, runTmuxNvim)

type HandlerStack =
  HandlerEffects ++ TmuxStack

type EmbedTmuxWith r =
  TestEffects ++ r ++ HandlerStack

type EmbedTmux =
  EmbedTmuxWith '[]

runEmbedTmuxTestConf ::
  HasCallStack =>
  TmuxTestConfig ->
  Sem HandlerStack () ->
  UnitTest
runEmbedTmuxTestConf conf =
  runTmuxNvim conf .
  interpretPluginEmbed

runEmbedTmuxTest ::
  HasCallStack =>
  Sem HandlerStack () ->
  UnitTest
runEmbedTmuxTest =
  runEmbedTmuxTestConf def

runEmbedTmuxGuiTest ::
  HasCallStack =>
  Sem HandlerStack () ->
  UnitTest
runEmbedTmuxGuiTest =
  runEmbedTmuxTestConf (def & #tmux . #gui .~ True)

testPluginEmbedTmuxConf ::
  ∀ r .
  HasCallStack =>
  Members HandlerStack (r ++ HandlerStack) =>
  TmuxTestConfig ->
  InterpretersFor r HandlerStack ->
  [RpcHandler (r ++ HandlerStack)] ->
  Sem (EmbedTmuxWith r) () ->
  UnitTest
testPluginEmbedTmuxConf conf effs handlers =
  runEmbedTmuxTestConf conf .
  effs .
  withHandlers handlers .
  testPluginEmbed

testPluginEmbedTmux ::
  ∀ r .
  HasCallStack =>
  Members HandlerStack (r ++ HandlerStack) =>
  InterpretersFor r HandlerStack ->
  [RpcHandler (r ++ HandlerStack)] ->
  Sem (EmbedTmuxWith r) () ->
  UnitTest
testPluginEmbedTmux =
  testPluginEmbedTmuxConf @r def

testPluginEmbedTmux_ ::
  HasCallStack =>
  [RpcHandler HandlerStack] ->
  Sem EmbedTmux () ->
  UnitTest
testPluginEmbedTmux_ =
  testPluginEmbedTmux @'[] id

testEmbedTmux ::
  HasCallStack =>
  Sem EmbedTmux () ->
  UnitTest
testEmbedTmux =
  testPluginEmbedTmux_ mempty
