module Ribosome.Test.EmbedTmux where

import Hedgehog (TestT)
import Polysemy.Test (SkipTestDefaultValue)

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
  SkipTestDefaultValue a =>
  TmuxTestConfig ->
  Sem HandlerStack a ->
  TestT IO a
runEmbedTmuxTestConf conf =
  runTmuxNvim conf .
  interpretPluginEmbed

runEmbedTmuxTest ::
  HasCallStack =>
  SkipTestDefaultValue a =>
  Sem HandlerStack a ->
  TestT IO a
runEmbedTmuxTest =
  runEmbedTmuxTestConf def

runEmbedTmuxGuiTest ::
  HasCallStack =>
  SkipTestDefaultValue a =>
  Sem HandlerStack a ->
  TestT IO a
runEmbedTmuxGuiTest =
  runEmbedTmuxTestConf (def & #tmux . #gui .~ True)

testPluginEmbedTmuxConf ::
  ∀ r a .
  HasCallStack =>
  SkipTestDefaultValue a =>
  Members HandlerStack (r ++ HandlerStack) =>
  TmuxTestConfig ->
  InterpretersFor r HandlerStack ->
  [RpcHandler (r ++ HandlerStack)] ->
  Sem (EmbedTmuxWith r) a ->
  TestT IO a
testPluginEmbedTmuxConf conf effs handlers =
  runEmbedTmuxTestConf conf .
  effs .
  withHandlers handlers .
  testPluginEmbed

testPluginEmbedTmux ::
  ∀ r a .
  HasCallStack =>
  SkipTestDefaultValue a =>
  Members HandlerStack (r ++ HandlerStack) =>
  InterpretersFor r HandlerStack ->
  [RpcHandler (r ++ HandlerStack)] ->
  Sem (EmbedTmuxWith r) a ->
  TestT IO a
testPluginEmbedTmux =
  testPluginEmbedTmuxConf @r def

testPluginEmbedTmux_ ::
  HasCallStack =>
  SkipTestDefaultValue a =>
  [RpcHandler HandlerStack] ->
  Sem EmbedTmux a ->
  TestT IO a
testPluginEmbedTmux_ =
  testPluginEmbedTmux @'[] id

testEmbedTmux ::
  HasCallStack =>
  SkipTestDefaultValue a =>
  Sem EmbedTmux a ->
  TestT IO a
testEmbedTmux =
  testPluginEmbedTmux_ mempty
