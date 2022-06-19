module Ribosome.Test.TmuxCommon where

import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Effect.TmuxClient (NativeTmux)
import qualified Chiasma.Test.Data.TmuxTestConfig as Chiasma
import Chiasma.Test.Tmux (TestTmuxEffects, withSystemTempDir, withTestTmux)
import Polysemy.Test (UnitTest)

import Ribosome.Data.PluginName (PluginName)
import Ribosome.Host.Data.BootError (BootError (BootError))
import Ribosome.Host.Test.Run (TestStack)
import Ribosome.Test.Data.TestConfig (TmuxTestConfig (TmuxTestConfig))
import Ribosome.Test.Embed (runTestConf)

type TmuxErrors =
  [
    Stop CodecError,
    Error CodecError,
    Stop RenderError,
    Error RenderError,
    Stop TmuxError,
    Error TmuxError,
    Error Text
  ]

type TmuxBaseStack =
  TestTmuxEffects ++ TmuxErrors

type TmuxStack =
  NativeTmux : TmuxBaseStack ++ Reader PluginName : TestStack

interpretTmuxErrors ::
  Member (Error BootError) r =>
  InterpretersFor TmuxErrors r
interpretTmuxErrors =
  mapError BootError .
  mapError @TmuxError (BootError . show) .
  stopToError .
  mapError @RenderError (BootError . show) .
  stopToError .
  mapError @CodecError (BootError . show) .
  stopToError

withTmuxTest ::
  Members TestStack r =>
  Chiasma.TmuxTestConfig ->
  InterpretersFor TmuxBaseStack r
withTmuxTest conf =
  interpretTmuxErrors .
  withSystemTempDir .
  withTestTmux conf

runTmuxNvim ::
  HasCallStack =>
  TmuxTestConfig ->
  Sem TmuxStack () ->
  UnitTest
runTmuxNvim (TmuxTestConfig conf tmuxConf) =
  runTestConf conf .
  withTmuxTest tmuxConf .
  restop @TmuxError @NativeTmux
