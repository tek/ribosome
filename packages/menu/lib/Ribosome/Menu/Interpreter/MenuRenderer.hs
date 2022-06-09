module Ribosome.Menu.Interpreter.MenuRenderer where

import Conc (interpretAtomic)
import Control.Lens ((<>~), (^.))
import qualified Data.Text as Text
import qualified Log
import Polysemy.Conc.Interpreter.Scoped (interpretScopedWith)

import Ribosome.Api.Window (closeWindow)
import Ribosome.Data.ScratchId (ScratchId)
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Data.WindowConfig (WindowConfig (WindowConfig))
import qualified Ribosome.Effect.Scratch as Scratch
import Ribosome.Effect.Scratch (Scratch)
import qualified Ribosome.Effect.Settings as Settings
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Api.Data (Window)
import Ribosome.Host.Api.Effect (nvimWinGetConfig, vimGetWindows, windowSetOption)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Menu.Data.NvimMenuState (NvimMenuState)
import Ribosome.Menu.Effect.MenuRenderer (MenuRenderer (MenuRender, MenuRenderQuit))
import Ribosome.Menu.NvimRenderer (menuSyntax, renderNvimMenu)
import qualified Ribosome.Menu.Settings as Settings

isFloat ::
  Member (Rpc !! RpcError) r =>
  Window ->
  Sem r Bool
isFloat win =
  False <! (check <$> nvimWinGetConfig win)
  where
    check (WindowConfig relative _ _) =
      not (Text.null relative)

closeFloats ::
  Members [Rpc, Rpc !! RpcError] r =>
  Sem r ()
closeFloats = do
  traverse_ closeWindow =<< filterM isFloat =<< vimGetWindows

withScratch ::
  Members [Settings !! SettingError, Scratch, Rpc, Rpc !! RpcError, Log, Resource, Embed IO] r =>
  ScratchOptions ->
  (ScratchId -> Sem (AtomicState NvimMenuState : r) a) ->
  Sem r a
withScratch options use = do
  whenM (Settings.or True Settings.menuCloseFloats) closeFloats
  bracket acquire Scratch.kill (interpretAtomic def . use)
  where
    acquire = do
      scratch <- Scratch.open (withSyntax options)
      windowSetOption (scratch ^. #window) "cursorline" True !>> Log.debug "Failed to set cursorline"
      pure (scratch ^. #id)
    withSyntax =
      #syntax <>~ [menuSyntax]

-- TODO remove Quit/kill since it is done in the bracket release function
interpretMenuRendererNvim ::
  Members [Settings !! SettingError, Scratch, Rpc, Rpc !! RpcError, Log, Resource, Embed IO, Final IO] r =>
  ScratchOptions ->
  InterpreterFor (Scoped ScratchId (MenuRenderer i)) r
interpretMenuRendererNvim options =
  interpretScopedWith @'[AtomicState NvimMenuState] (withScratch options) \ scratchId -> \case
    MenuRender menu -> do
      Scratch.find scratchId >>= traverse_ (runReader menu . renderNvimMenu)
    MenuRenderQuit ->
      Scratch.kill scratchId
