{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Ribosome.Control.Monad.Ribo where

import Control.Concurrent.STM.TVar (modifyTVar)
import Control.Lens (Lens')
import qualified Control.Lens as Lens (over, set, view)
import Control.Monad (join, (<=<))
import Control.Monad.Base (MonadBase(..), liftBaseDefault)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.DeepError (MonadDeepError(throwHoist))
import Control.Monad.DeepState (MonadDeepState, gets, modify)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO(..), UnliftIO(..), withUnliftIO)
import Control.Monad.Reader.Class (MonadReader, ask, asks)
import Control.Monad.State.Class (MonadState(put, get))
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Control (
  ComposeSt,
  MonadBaseControl(..),
  MonadTransControl(..),
  defaultLiftBaseWith,
  defaultRestoreM,
  )
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT, withExceptT)
import Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Either (fromRight)
import Data.Either.Combinators (mapLeft)
import Data.Functor (void)
import Neovim.Context.Internal (Neovim(..))
import Ribosome.Plugin (RpcHandler(..))
import UnliftIO.STM (TVar, atomically, readTVarIO)

import Ribosome.Control.Ribosome (Ribosome(Ribosome), RibosomeInternal, RibosomeState)
import qualified Ribosome.Control.Ribosome as Ribosome (_errors, errors, name, state)
import qualified Ribosome.Control.Ribosome as RibosomeState (internal, public)
import Ribosome.Data.Errors (Errors)
import Ribosome.Nvim.Api.RpcCall (Rpc, RpcError)
import qualified Ribosome.Nvim.Api.RpcCall as Rpc (Rpc(..))

type ConcNvimS e = Neovim (Ribosome e)

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

-- FIXME change get/put to get/modify to avoid racing
data RiboConcState s =
  RiboConcState {
    rsaName :: String,
    rsaInternalGet :: IO RibosomeInternal,
    rsaInternalPut :: RibosomeInternal -> IO (),
    rsaGet :: IO s,
    rsaPut :: s -> IO ()
  }

ribLocal :: Lens' s s' -> RiboConcState s -> RiboConcState s'
ribLocal lens (RiboConcState n ig ip g p) =
  RiboConcState n ig ip (Lens.view lens <$> g) (\s' -> g >>= p . Lens.set lens s')

newtype Ribo s m a =
  Ribo { unRibo :: ReaderT (RiboConcState s) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

type RiboE s e m = Ribo s (ExceptT e m)
type RiboN s e a = RiboE s e (ConcNvimS s) a

instance MonadIO m => MonadState s (Ribo s m) where
  get = do
    RiboConcState{..} <- Ribo ask
    liftIO rsaGet
  put s = do
    RiboConcState{..} <- Ribo ask
    liftIO $ rsaPut s

instance MonadTrans (Ribo s) where
  lift = Ribo . lift

instance MonadUnliftIO m => MonadUnliftIO (Ribo s m) where
  askUnliftIO = Ribo . withUnliftIO $ \x -> return (UnliftIO (unliftIO x . unRibo))

class Nvim m where
  call :: Monad m => Rpc c a => c -> m (Either RpcError a)

instance Nvim (Neovim e) where
  call = Rpc.call

class (Nvim m, MonadDeepError e RpcError m) => NvimE e m where

instance (Nvim m, MonadDeepError e RpcError m) => NvimE e m where

instance (MonadTrans t, Nvim m, Monad m) => Nvim (t m) where
  call = lift . call

instance MonadError e m => MonadError e (Ribo s m) where
  throwError e = Ribo $ lift $ throwError e
  catchError ma f =
    Ribo $ ReaderT (\r -> catchError (runReaderT (unRibo ma) r) ((`runReaderT` r) . unRibo . f))

instance (MonadBase b m) => MonadBase b (Ribo s m) where
  liftBase = liftBaseDefault

instance MonadTransControl (Ribo s) where
    type StT (Ribo s) a = a
    liftWith f =
      Ribo $ ReaderT $ \r -> f $ \t -> runReaderT (unRibo t) r
    restoreT = Ribo . restoreT
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance (MonadBaseControl b m) => MonadBaseControl b (Ribo s m) where
    type StM (Ribo s m) a = ComposeSt (Ribo s) m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM
    {-# INLINABLE liftBaseWith #-}
    {-# INLINABLE restoreM #-}

instance RpcHandler e (Ribosome s) (RiboE s e (ConcNvimS s)) where
  native = runRib

acall :: (Monad m, Nvim m, Rpc c ()) => c -> m ()
acall c = fromRight () <$> call c

writeTv :: Lens' (RibosomeState s) s' -> TVar (RibosomeState s) -> s' -> IO ()
writeTv l t = void . atomically . modifyTVar t . Lens.set l

readTv :: Lens' (RibosomeState s) s' -> TVar (RibosomeState s) -> IO s'
readTv l t = Lens.view l <$> readTVarIO t

ribId :: MonadReader (Ribosome s) m => m (RiboConcState s)
ribId = do
  Ribosome n tv <- ask
  return $ RiboConcState n (readTv RibosomeState.internal tv) (writeTv int tv) (readTv pub tv) (writeTv pub tv)
  where
    int :: Lens' (RibosomeState s) RibosomeInternal
    int = RibosomeState.internal
    pub :: Lens' (RibosomeState s) s
    pub = RibosomeState.public

runRib :: MonadReader (Ribosome s) m => Ribo s m a -> m a
runRib ma = do
  rib0 <- ribId
  (`runReaderT` rib0) . unRibo $ ma

local :: Monad m => Lens' s s' -> Ribo s' m a -> Ribo s m a
local lens ma = do
  r <- Ribo ask
  Ribo . lift . runReaderT (unRibo ma) $ ribLocal lens r

unliftRibo :: (m a -> n b) -> Ribo s m a -> Ribo s n b
unliftRibo f ma =
  Ribo $ ReaderT (f . runReaderT (unRibo ma))

riboE :: Ribo s m (Either e a) -> RiboE s e m a
riboE =
  unliftRibo ExceptT

liftRibo :: Functor m => Ribo s m a -> RiboE s e m a
liftRibo =
  riboE . fmap Right

runRiboE ::
  MonadReader (Ribosome s) m =>
  RiboE s e m a ->
  m (Either e a)
runRiboE ma = do
  rib0 <- ribId
  runExceptT $ (`runReaderT` rib0) . unRibo $ ma

unliftRiboE :: (ExceptT e m a -> ExceptT e' n b) -> RiboE s e m a -> RiboE s e' n b
unliftRiboE f ma =
  Ribo $ ReaderT (f . runReaderT (unRibo ma))

riboE2ribo :: RiboE s e m a -> Ribo s m (Either e a)
riboE2ribo ma =
  Ribo $ ReaderT (runExceptT . runReaderT (unRibo ma))

mapEither :: Monad m => (Either e a -> RiboE s e' m b) -> RiboE s e m a -> RiboE s e' m b
mapEither f =
  join . unliftRiboE (lift . (f <$>) . runExceptT)

mapE :: Functor m => (e -> e') -> RiboE s e m a -> RiboE s e' m a
mapE =
  unliftRiboE . withExceptT

catchE :: Monad m => (e -> RiboE s e' m a) -> RiboE s e m a -> RiboE s e' m a
catchE f =
  mapEither (either f (lift . return))

anaE :: Functor m => (e -> e') -> RiboE s e m (Either e' a) -> RiboE s e' m a
anaE f =
  riboE . fmap (join . mapLeft f) . riboE2ribo

cataE :: Functor m => (e' -> e) -> RiboE s e m (Either e' a) -> RiboE s e m a
cataE f =
  riboE . fmap join . riboE2ribo . fmap (mapLeft f)

riboInternal :: (MonadReader (RiboConcState s) m, MonadIO m) => m RibosomeInternal
riboInternal =
  liftIO =<< asks rsaInternalGet

class (MonadIO m, Nvim m) => MonadRibo m where
  pluginName :: m String
  pluginInternal :: m RibosomeInternal
  pluginInternalPut :: RibosomeInternal -> m ()

pluginInternals :: MonadRibo m => (RibosomeInternal -> a) -> m a
pluginInternals = (<$> pluginInternal)

pluginInternalL :: MonadRibo m => Lens' RibosomeInternal a -> m a
pluginInternalL = pluginInternals . Lens.view

pluginModifyInternal :: MonadRibo m => Lens' RibosomeInternal a -> (a -> a) -> m ()
pluginModifyInternal l f = do
  cur <- pluginInternal
  pluginInternalPut $ Lens.over l f cur

instance (MonadIO m, Nvim m) => MonadRibo (Ribo s m) where
  pluginName =
    Ribo (asks rsaName)

  pluginInternal =
    liftIO <=< Ribo $ asks rsaInternalGet

  pluginInternalPut i = do
    p <- Ribo $ asks rsaInternalPut
    liftIO . p $ i

instance MonadRibo m => MonadRibo (ExceptT e m) where
  pluginName = lift pluginName
  pluginInternal = lift pluginInternal
  pluginInternalPut = lift . pluginInternalPut

instance MonadRibo (ConcNvimS e) where
  pluginName =
    asks $ Lens.view Ribosome.name

  pluginInternal = do
    tv <- asks $ Lens.view Ribosome.state
    Lens.view RibosomeState.internal <$> readTVarIO tv

  pluginInternalPut i = do
    tv <- asks $ Lens.view Ribosome.state
    atomically $ modifyTVar tv $ Lens.set RibosomeState.internal i

getErrors :: MonadRibo m => m Errors
getErrors =
  pluginInternals Ribosome._errors

inspectErrors :: MonadRibo m => (Errors -> a) -> m a
inspectErrors = (<$> getErrors)

modifyErrors :: MonadRibo m => (Errors -> Errors) -> m ()
modifyErrors =
  pluginModifyInternal Ribosome.errors

prepend :: ∀s' s m a. MonadDeepState s s' m => Lens' s' [a] -> a -> m ()
prepend lens a =
  modify $ Lens.over lens (a:)

inspectHeadE :: (MonadDeepState s s' m, MonadDeepError e e' m) => e' -> Lens' s' [a] -> m a
inspectHeadE err lens = do
  as <- gets $ Lens.view lens
  case as of
    (a : _) -> return a
    _ -> throwHoist err
