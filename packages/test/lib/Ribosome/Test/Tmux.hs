module Ribosome.Test.Tmux where

import Chiasma.Command.Pane (sendKeys)
import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.SendKeysParams (Key (Lit))
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Effect.TmuxApi (Tmux)
import Chiasma.Effect.TmuxClient (NativeTmux)
import Chiasma.Test.Tmux (TestTmuxEffects, TmuxTestConf (ttcGui), withSystemTempDir, withTestTmux)
import Chiasma.Tmux (withTmux)
import Data.MessagePack (Object)
import Exon (exon)
import Hedgehog.Internal.Property (Failure)
import Path (Abs, File, Path, reldir, relfile, (</>))
import Path.IO (doesPathExist)
import Polysemy.Chronos (ChronosTime)
import qualified Polysemy.Test as Test
import Polysemy.Test (Hedgehog, Test, UnitTest, assert)

import Ribosome.Data.Mapping (MappingIdent)
import Ribosome.Data.PluginConfig (PluginConfig (..))
import Ribosome.Data.WatchedVariable (WatchedVariable)
import Ribosome.Host.Data.BootError (BootError (BootError))
import Ribosome.Host.Data.NvimSocket (NvimSocket (NvimSocket))
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import qualified Ribosome.Host.Test.Data.TestConfig as Host
import Ribosome.Host.Test.Run (TestStack, runTestConf)
import Ribosome.Path (pathText)
import Ribosome.Socket (PluginHandler, PluginSocketStack, TestPluginSocketStack, socketNvimPlugin)
import Ribosome.Test.Data.TestConfig (TestConfig (TestConfig))
import Ribosome.Test.Wait (assertWait)

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

type TmuxStack =
  TestTmuxEffects ++ TmuxErrors

type TmuxStack' =
  Reader NvimSocket : Tmux : TmuxStack

type TmuxTestStack =
  TmuxStack' ++ TestStack

type TmuxHandler =
  PluginHandler TmuxTestStack

type TmuxPluginStack =
  PluginSocketStack ++ TmuxTestStack

type TestTmuxPluginStack =
  TestPluginSocketStack ++ TmuxTestStack

tmuxConf :: TmuxTestConf
tmuxConf =
  def { ttcGui = True }

withTmuxTest ::
  Members TestStack r =>
  TmuxTestConf ->
  InterpretersFor TmuxStack r
withTmuxTest conf =
  mapError BootError .
  mapError @TmuxError (BootError . show) .
  stopToError .
  mapError @RenderError (BootError . show) .
  stopToError .
  mapError @CodecError (BootError . show) .
  stopToError .
  withSystemTempDir .
  withTestTmux conf

nvimCmdline :: Path Abs File -> Text
nvimCmdline socket =
  [exon|nvim --listen #{pathText socket} -n -u NONE -i NONE --clean|]

withTmuxNvim ::
  Members [Tmux, Test, Hedgehog IO, ChronosTime, Error Failure, Race, Embed IO] r =>
  InterpreterFor (Reader NvimSocket) r
withTmuxNvim sem = do
  dir <- Test.tempDir [reldir|tmux-test|]
  let socket = dir </> [relfile|nvim-socket|]
  sendKeys 0 [Lit (nvimCmdline socket)]
  assertWait (pure socket) (assert <=< doesPathExist)
  runReader (NvimSocket socket) sem

tmuxPluginTestConf ::
  TestConfig ->
  [RpcHandler (PluginSocketStack ++ TmuxTestStack)] ->
  Map MappingIdent TmuxHandler ->
  Map WatchedVariable (Object -> TmuxHandler) ->
  Sem TestTmuxPluginStack () ->
  UnitTest
tmuxPluginTestConf (TestConfig freeze (PluginConfig name conf)) handlers maps vars =
  runTestConf (Host.TestConfig freeze conf) .
  withTmuxTest tmuxConf .
  restop @TmuxError @NativeTmux .
  withTmux .
  restop @CodecError @Tmux .
  raiseUnder2 .
  withTmuxNvim .
  socketNvimPlugin handlers name maps vars
