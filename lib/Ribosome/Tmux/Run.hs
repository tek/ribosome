module Ribosome.Tmux.Run where

import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Monad.Stream (TmuxProg)
import qualified Chiasma.Monad.Stream as Chiasma (runTmux)
import Chiasma.Native.Api (TmuxNative(TmuxNative))
import Control.Monad.Catch (MonadMask)
import Control.Monad.DeepError (MonadDeepError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.DeepPrisms (DeepPrisms)

import Ribosome.Config.Setting (settingMaybe)
import Ribosome.Config.Settings (tmuxSocket)
import Ribosome.Control.Monad.Ribo (MonadRibo, Nvim, RiboE)

runTmux ::
  (MonadIO m, MonadRibo m, MonadDeepError e TmuxError m, MonadMask m, Nvim m) =>
  TmuxProg m a ->
  m a
runTmux prog = do
  socket <- settingMaybe tmuxSocket
  Chiasma.runTmux (TmuxNative socket) prog

runTmuxE ::
  (MonadIO m, MonadRibo m, MonadMask m, Nvim m) =>
  TmuxProg (ExceptT TmuxError m) a ->
  m (Either TmuxError a)
runTmuxE =
  runExceptT . runTmux

class RunTmux m where
  runRiboTmux :: TmuxProg m b -> m b

instance (MonadIO m, MonadRibo m, DeepPrisms e TmuxError, MonadMask m, Nvim m) =>
  RunTmux (RiboE s e m) where
    runRiboTmux = runTmux
