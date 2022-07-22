-- |Socket tmux tests start a tmux server, then run a regular Neovim session in a pane and connect to Neovim over a
-- socket.
module Ribosome.Test.SocketTmux where

import Chiasma.Command.Pane (sendKeys)
import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.SendKeysParams (Key (Lit))
import Chiasma.Effect.Codec (NativeCommandCodecE)
import Chiasma.Effect.TmuxApi (Tmux)
import Chiasma.Effect.TmuxClient (NativeTmux)
import Chiasma.Tmux (withTmux)
import Exon (exon)
import Hedgehog.Internal.Property (Failure)
import Path (Abs, File, Path, reldir, relfile, (</>))
import Path.IO (doesPathExist)
import Polysemy.Chronos (ChronosTime)
import qualified Polysemy.Test as Test
import Polysemy.Test (Hedgehog, Test, UnitTest, assert)

import Ribosome.Host.Data.NvimSocket (NvimSocket (NvimSocket))
import Ribosome.Host.Data.RpcHandler (RpcHandler)
import Ribosome.Host.Interpreter.Handlers (withHandlers)
import Ribosome.Path (pathText)
import Ribosome.Socket (SocketHandlerEffects, interpretPluginSocket)
import Ribosome.Test.Data.TestConfig (TmuxTestConfig)
import Ribosome.Test.Embed (TestEffects, testPluginEmbed)
import Ribosome.Test.TmuxCommon (TmuxStack, runTmuxNvim)
import Ribosome.Test.Wait (assertWait)

-- |The stack of internal effects for socket tmux tests.
type TmuxHandlerStack =
  SocketHandlerEffects ++ Reader NvimSocket : TmuxStack

-- |The socket tmux test stack with additional effects.
type SocketTmuxWith r =
  TestEffects ++ r ++ TmuxHandlerStack

-- |The socket tmux test stack with no additional effects.
type SocketTmux =
  SocketTmuxWith '[]

nvimCmdline :: Path Abs File -> Text
nvimCmdline socket =
  [exon|nvim --listen #{pathText socket} -n -u NONE -i NONE --clean|]

-- |Create a socket for Neovim to listen on and run a 'Reader' with it.
withSocketTmuxNvim ::
  Members [Test, Hedgehog IO, ChronosTime, Error Failure, Race, Embed IO] r =>
  Members [NativeTmux, NativeCommandCodecE, Stop CodecError] r =>
  InterpreterFor (Reader NvimSocket) r
withSocketTmuxNvim sem = do
  dir <- Test.tempDir [reldir|tmux-test|]
  let socket = dir </> [relfile|nvim-socket|]
  withTmux do
    restop @CodecError @Tmux $ sendKeys 0 [Lit (nvimCmdline socket)]
  assertWait (pure socket) (assert <=< doesPathExist)
  runReader (NvimSocket socket) sem

-- |Run the tmux test stack.
runSocketTmuxTestConf ::
  HasCallStack =>
  TmuxTestConfig ->
  Sem TmuxHandlerStack () ->
  UnitTest
runSocketTmuxTestConf conf =
  runTmuxNvim conf .
  withSocketTmuxNvim .
  interpretPluginSocket

-- |Run the tmux test stack, using a pty to host tmux.
runSocketTmuxTest ::
  HasCallStack =>
  Sem TmuxHandlerStack () ->
  UnitTest
runSocketTmuxTest =
  runSocketTmuxTestConf def

-- |Run the tmux test stack, using a terminal to tmux tmux.
runSocketTmuxGuiTest ::
  HasCallStack =>
  Sem TmuxHandlerStack () ->
  UnitTest
runSocketTmuxGuiTest =
  runSocketTmuxTestConf (def & #tmux . #gui .~ True)

-- |Run a plugin test against a Neovim process running in a fresh tmux session, connected over a socket.
testPluginSocketTmuxConf ::
  ∀ r .
  HasCallStack =>
  Members TmuxHandlerStack (r ++ TmuxHandlerStack) =>
  -- |Regular test config combined with tmux config
  TmuxTestConfig ->
  -- |Interpreter for custom effects
  InterpretersFor r TmuxHandlerStack ->
  -- |RPC handlers
  [RpcHandler (r ++ TmuxHandlerStack)] ->
  Sem (SocketTmuxWith r) () ->
  UnitTest
testPluginSocketTmuxConf conf effs handlers =
  runSocketTmuxTestConf conf .
  effs .
  withHandlers handlers .
  testPluginEmbed

-- |Run a plugin test against a Neovim process running in a fresh tmux session, connected over a socket.
testPluginSocketTmux ::
  ∀ r .
  HasCallStack =>
  Members TmuxHandlerStack (r ++ TmuxHandlerStack) =>
  -- |Interpreter for custom effects
  InterpretersFor r TmuxHandlerStack ->
  -- |RPC handlers
  [RpcHandler (r ++ TmuxHandlerStack)] ->
  Sem (SocketTmuxWith r) () ->
  UnitTest
testPluginSocketTmux =
  testPluginSocketTmuxConf @r def

-- |Run a plugin test against a Neovim process running in a fresh tmux session, connected over a socket.
testHandlersSocketTmux ::
  HasCallStack =>
  -- |RPC handlers
  [RpcHandler TmuxHandlerStack] ->
  Sem SocketTmux () ->
  UnitTest
testHandlersSocketTmux =
  testPluginSocketTmux @'[] id

-- |Run a plugin test against a Neovim process running in a fresh tmux session, connected over a socket.
testSocketTmux ::
  HasCallStack =>
  Sem SocketTmux () ->
  UnitTest
testSocketTmux =
  testHandlersSocketTmux mempty
