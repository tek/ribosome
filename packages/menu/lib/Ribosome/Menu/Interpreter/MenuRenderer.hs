module Ribosome.Menu.Interpreter.MenuRenderer where

import Conc (PScoped, interpretAtomic, interpretPScopedResumableWith)
import qualified Data.Text as Text
import qualified Log

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
import Ribosome.Menu.Effect.MenuRenderer (MenuRenderer (MenuRender))
import Ribosome.Menu.NvimRenderer (menuSyntax, renderNvimMenu)
import qualified Ribosome.Menu.Settings as Settings

interpretMenuRendererNull :: InterpreterFor (MenuRenderer i) r
interpretMenuRendererNull =
  interpret \case
    MenuRender _ ->
      unit

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
  Members [Settings !! SettingError, Rpc !! RpcError, Scratch !! RpcError, Log, Resource, Embed IO] r =>
  ScratchOptions ->
  (ScratchId -> Sem (AtomicState NvimMenuState : Stop RpcError : r) a) ->
  Sem (Stop RpcError : r) a
withScratch options use = do
  whenM (Settings.or True Settings.menuCloseFloats) (restop @_ @Rpc closeFloats)
  bracket acquire (resume_ . Scratch.kill) (interpretAtomic def . use)
  where
    acquire = do
      scratch <- restop (Scratch.open (withSyntax options))
      windowSetOption (scratch ^. #window) "cursorline" True !>> Log.debug "Failed to set cursorline"
      pure (scratch ^. #id)
    withSyntax =
      #syntax <>~ [menuSyntax]

-- TODO remove Quit/kill since it is done in the bracket release function
interpretMenuRendererNvim ::
  Members [Settings !! SettingError, Scratch !! RpcError, Rpc !! RpcError, Log, Resource, Embed IO] r =>
  InterpreterFor (PScoped ScratchOptions ScratchId (MenuRenderer i) !! RpcError) r
interpretMenuRendererNvim =
  interpretPScopedResumableWith @'[AtomicState NvimMenuState] withScratch \ scratchId -> \case
    MenuRender menu ->
      restop (Scratch.find scratchId) >>= traverse_ (runReader menu . renderNvimMenu)
