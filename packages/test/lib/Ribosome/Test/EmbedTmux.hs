module Ribosome.Test.EmbedTmux where

import Polysemy.Test (UnitTest)

import Ribosome.Effect.NvimPlugin (NvimPlugin)
import Ribosome.Embed (HandlerEffects, interpretPluginEmbed)
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Interpreter.NvimPlugin (rpcHandlers)
import Ribosome.Test.Data.TestConfig (TmuxTestConfig)
import Ribosome.Test.Embed (EmbedEffects, testPluginEmbed)
import Ribosome.Test.TmuxCommon (TmuxStack, runTmuxNvim)

type HandlerStack =
  HandlerEffects ++ TmuxStack

type EmbedTmuxWith r =
  EmbedEffects ++ r ++ HandlerStack

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
  InterpretersFor (NvimPlugin ++ r) HandlerStack ->
  Sem (EmbedTmuxWith r) () ->
  UnitTest
testPluginEmbedTmuxConf conf handlers =
  runEmbedTmuxTestConf conf .
  handlers .
  testPluginEmbed

testPluginEmbedTmux ::
  ∀ r .
  HasCallStack =>
  Members HandlerStack (r ++ HandlerStack) =>
  InterpretersFor (NvimPlugin ++ r) HandlerStack ->
  Sem (EmbedTmuxWith r) () ->
  UnitTest
testPluginEmbedTmux =
  testPluginEmbedTmuxConf @r def

testPluginEmbedTmux_ ::
  HasCallStack =>
  InterpretersFor NvimPlugin HandlerStack ->
  Sem EmbedTmux () ->
  UnitTest
testPluginEmbedTmux_ =
  testPluginEmbedTmux @'[]

testHandlersEmbedTmux ::
  HasCallStack =>
  [RpcHandler HandlerStack] ->
  Sem EmbedTmux () ->
  UnitTest
testHandlersEmbedTmux handlers =
  testPluginEmbedTmux @'[] (rpcHandlers handlers)

testEmbedTmux ::
  HasCallStack =>
  Sem EmbedTmux () ->
  UnitTest
testEmbedTmux =
  testHandlersEmbedTmux mempty
