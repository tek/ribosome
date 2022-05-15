module Ribosome.Tmux.Run where

import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Monad.Stream (TmuxProg)
import qualified Chiasma.Monad.Stream as Chiasma (runTmux)
import Chiasma.Native.Api (TmuxNative(TmuxNative))
import Control.Monad.Catch (MonadMask)
import Data.DeepPrisms (DeepPrisms)

import Ribosome.Config.Setting (settingMaybe)
import Ribosome.Config.Settings (tmuxSocket)

runTmux ::
  MonadIO m =>
  MonadMask m =>
  Nvim m =>
  TmuxProg m a ->
  m a
runTmux prog = do
  socket <- settingMaybe tmuxSocket
  Chiasma.runTmux (TmuxNative socket) prog

runTmuxE ::
  MonadIO m =>
  MonadMask m =>
  Nvim m =>
  TmuxProg (ExceptT TmuxError m) a ->
  m (Either TmuxError a)
runTmuxE =
  runExceptT . runTmux

class RunTmux m where
  runRiboTmux :: TmuxProg m b -> m b

instance DeepPrisms e TmuxError => RunTmux (Ribo s e) where
    runRiboTmux = runTmux
