{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ribosome.Control.Monad.Ribo where

import Control.Concurrent.STM.TMVar (putTMVar, readTMVar, takeTMVar)
import Control.Exception.Lifted (onException)
import Control.Lens (Lens')
import qualified Control.Lens as Lens (mapMOf, over, view)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.DeepError (MonadDeepError(throwHoist))
import Control.Monad.DeepState (MonadDeepState(modifyM))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.Trans.Free (FreeT)
import Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.DeepLenses (DeepLenses(deepLens))
import Data.DeepPrisms (DeepPrisms)
import Neovim.Context.Internal (Neovim(..))
import Ribosome.Plugin.RpcHandler (RpcHandler(..))
import UnliftIO.STM (TMVar)

import Ribosome.Control.Ribosome (Ribosome, RibosomeInternal, RibosomeState)
import qualified Ribosome.Control.Ribosome as Ribosome (_errors, errors, name, state)
import qualified Ribosome.Control.Ribosome as RibosomeState (internal, public)
import Ribosome.Data.Errors (Errors)
import Ribosome.Nvim.Api.RpcCall (Rpc, RpcError)
import qualified Ribosome.Nvim.Api.RpcCall as Rpc (Rpc(..))
import Ribosome.Orphans ()

type RNeovim s = Neovim (Ribosome s)

instance MonadBase IO (Neovim e) where
  liftBase = liftIO

instance MonadBaseControl IO (Neovim e) where
  type StM (Neovim e) a = a
  liftBaseWith f =
    Neovim (lift $ ReaderT $ \r -> f (peel r))
    where
      peel r ma =
        runReaderT (runResourceT (unNeovim ma)) r
  restoreM = return

newtype Ribo s e a =
  Ribo { unRibo :: ExceptT e (RNeovim s) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadFail, MonadBase IO)

deriving instance MonadError e (Ribo s e)

modifyTMVar ::
  MonadIO m =>
  MonadBaseControl IO m =>
  (a -> a) ->
  TMVar a ->
  m ()
modifyTMVar f tmvar =
  atomically . putTMVar tmvar =<< f <$> atomically (takeTMVar tmvar)

safeModifyTMVarM ::
  MonadIO m =>
  MonadBaseControl IO m =>
  (a -> m a) ->
  TMVar a ->
  m ()
safeModifyTMVarM f tmvar =
  process =<< atomically (takeTMVar tmvar)
  where
    process a =
      onException (restore =<< f a) (restore a)
    restore =
      atomically . putTMVar tmvar

deriving instance MonadReader (Ribosome s) (Ribo s e)

riboStateVar ::
  MonadReader (Ribosome s) m =>
  m (TMVar (RibosomeState s))
riboStateVar =
  asks (Lens.view Ribosome.state)

public ::
  DeepLenses s s' =>
  Lens' (RibosomeState s) s'
public =
  RibosomeState.public . deepLens

instance DeepLenses s s' => MonadDeepState s s' (Ribo s e) where
  get =
    Lens.view public <$> (atomically . readTMVar =<< riboStateVar)

  modifyM f =
    safeModifyTMVarM trans =<< riboStateVar
    where
      trans = Lens.mapMOf public f

  put =
    modify . const

class Nvim m where
  call :: Monad m => Rpc c a => c -> m (Either RpcError a)

instance Nvim (Neovim e) where
  call = Rpc.call

instance (Monad m, Nvim m) => Nvim (ExceptT e m) where
  call = lift . call

instance (Monad m, Nvim m) => Nvim (FreeT f m) where
  call = lift . call

instance Nvim (Ribo s e) where
  call = Ribo . call

class (Nvim m, MonadDeepError e RpcError m) => NvimE e m where

instance DeepPrisms e RpcError => NvimE e (Ribo s e) where

instance (DeepPrisms e RpcError, Nvim m, Monad m) => NvimE e (ExceptT e m) where

instance (Functor f, MonadDeepError e RpcError m, Nvim m, Monad m) => NvimE e (FreeT f m) where

instance MonadBaseControl IO (Ribo s e) where
    type StM (Ribo s e) a = Either e a

    liftBaseWith f =
      Ribo $ liftBaseWith $ \ q -> f (q . unRibo)

    restoreM =
      Ribo . restoreM

    {-# INLINABLE liftBaseWith #-}
    {-# INLINABLE restoreM #-}

instance RpcHandler e (Ribosome s) (Ribo s e) where
  native = runRiboE

acall :: (Monad m, Nvim m, Rpc c ()) => c -> m ()
acall c = fromRight () <$> call c

readTv :: Lens' (RibosomeState s) s' -> TMVar (RibosomeState s) -> IO s'
readTv l t = Lens.view l <$> atomically (readTMVar t)

runRibo :: Ribo s e a -> RNeovim s (Either e a)
runRibo =
  runExceptT . unRibo

runRiboE :: Ribo s e a -> ExceptT e (RNeovim s) a
runRiboE =
  unRibo

class (MonadIO m, Nvim m) => MonadRibo m where
  pluginName :: m Text
  pluginInternal :: m RibosomeInternal
  pluginInternalModify :: (RibosomeInternal -> RibosomeInternal) -> m ()

pluginInternals :: MonadRibo m => (RibosomeInternal -> a) -> m a
pluginInternals = (<$> pluginInternal)

pluginInternalL :: MonadRibo m => Lens' RibosomeInternal a -> m a
pluginInternalL = pluginInternals . Lens.view

pluginInternalPut' :: MonadRibo m => RibosomeInternal -> m ()
pluginInternalPut' s =
  pluginInternalModify (const s)

pluginInternalModifyL :: MonadRibo m => Lens' RibosomeInternal a -> (a -> a) -> m ()
pluginInternalModifyL l f =
  pluginInternalModify $ Lens.over l f

instance MonadRibo (RNeovim s) where
  pluginName =
    asks (Lens.view Ribosome.name)

  pluginInternal =
    Lens.view RibosomeState.internal <$$> atomically . readTMVar =<< asks (Lens.view Ribosome.state)

  pluginInternalModify f =
    modifyTMVar (Lens.over RibosomeState.internal f) =<< riboStateVar

instance MonadRibo m => MonadRibo (ExceptT e m) where
  pluginName = lift pluginName
  pluginInternal = lift pluginInternal
  pluginInternalModify = lift . pluginInternalModify

instance MonadRibo (Ribo s e) where
  pluginName = Ribo pluginName
  pluginInternal = Ribo pluginInternal
  pluginInternalModify = Ribo . pluginInternalModify

getErrors :: MonadRibo m => m Errors
getErrors =
  pluginInternals Ribosome._errors

inspectErrors :: MonadRibo m => (Errors -> a) -> m a
inspectErrors = (<$> getErrors)

modifyErrors :: MonadRibo m => (Errors -> Errors) -> m ()
modifyErrors =
  pluginInternalModifyL Ribosome.errors

prepend :: ∀s' s m a. MonadDeepState s s' m => Lens' s' [a] -> a -> m ()
prepend lens a =
  modify $ Lens.over lens (a:)

inspectHeadE ::
  ∀ s' s e e' m a .
  (MonadDeepState s s' m, MonadDeepError e e' m) =>
  e' ->
  Lens' s' [a] ->
  m a
inspectHeadE err lens = do
  as <- gets $ Lens.view lens
  case as of
    (a : _) -> return a
    _ -> throwHoist err
