{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Ribosome.Control.Monad.Ribo where

import Control.Concurrent.STM.TMVar (putTMVar, readTMVar, takeTMVar)
import Control.Lens (Lens')
import qualified Control.Lens as Lens (mapMOf, over, set, view)
import Control.Monad (join, (<=<))
import Control.Monad.Base (MonadBase(..), liftBaseDefault)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.DeepError (MonadDeepError(throwHoist))
import Control.Monad.DeepState (MonadDeepState(modifyE))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO(..), UnliftIO(..), withUnliftIO)
import Control.Monad.Reader.Class (MonadReader, ask, asks)
import qualified Control.Monad.State.Class as MS (MonadState(get), modify)
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
import Data.DeepLenses (DeepLenses(deepLens))
import Data.Either.Combinators (swapEither)
import Data.Either.Combinators (mapLeft)
import Data.Functor (void)
import Neovim.Context.Internal (Neovim(..))
import Ribosome.Plugin.RpcHandler (RpcHandler(..))
import UnliftIO.STM (TMVar)

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

data RiboConcState s =
  RiboConcState {
    rsaName :: Text,
    rsaInternalGet :: IO RibosomeInternal,
    rsaInternalModify :: ∀ e . (RibosomeInternal -> (Either e RibosomeInternal)) -> IO (Maybe e),
    rsaGet :: IO s,
    rsaModify :: ∀ e . (s -> (Either e s)) -> IO (Maybe e)
  }

ribLocal :: Lens' s s' -> RiboConcState s -> RiboConcState s'
ribLocal lens (RiboConcState n ig ip g m) =
  RiboConcState n ig ip (Lens.view lens <$> g) (m . Lens.mapMOf lens)
  -- where
  --   modi f =

newtype Ribo s m a =
  Ribo { unRibo :: ReaderT (RiboConcState s) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadFail)

type RiboE s e m = Ribo s (ExceptT e m)
type RiboN s e = RiboE s e (ConcNvimS s)

instance (Monad m, MonadIO m, DeepLenses s s') => MonadDeepState s s' (Ribo s m) where
  get = do
    RiboConcState{..} <- Ribo ask
    liftIO (Lens.view deepLens <$> rsaGet)

  modifyE f = do
    RiboConcState{..} <- Ribo ask
    liftIO (rsaModify $ Lens.mapMOf deepLens f)

  put =
    modify . const

-- instance MonadIO m => MonadState s (Ribo s m) where
--   get = do
--     RiboConcState{..} <- Ribo ask
--     liftIO rsaGet
--   put s = do
--     RiboConcState{..} <- Ribo ask
--     liftIO $ rsaModify (const s)

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

modifyTv :: Lens' (RibosomeState s) s' -> TMVar (RibosomeState s) -> (s' -> (Either e s')) -> IO (Maybe e)
modifyTv l t f =
  atomically g
  where
    g = do
      pre <- takeTMVar t
      let post = Lens.mapMOf l f pre
      putTMVar t (fromRight pre post)
      return . rightToMaybe . swapEither $ post

readTv :: Lens' (RibosomeState s) s' -> TMVar (RibosomeState s) -> IO s'
readTv l t = Lens.view l <$> atomically (readTMVar t)

ribId :: MonadReader (Ribosome s) m => m (RiboConcState s)
ribId = do
  Ribosome n tv <- ask
  return $ RiboConcState n (readTv RibosomeState.internal tv) (modifyTv int tv) (readTv pub tv) (modifyTv pub tv)
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

riboE2ribo ::
  ∀ e s m a.
  RiboE s e m a ->
  Ribo s m (Either e a)
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
  pluginName :: m Text
  pluginInternal :: m RibosomeInternal
  pluginInternalModifyE :: ∀ e . (RibosomeInternal -> Either e RibosomeInternal) -> m (Maybe e)

pluginInternals :: MonadRibo m => (RibosomeInternal -> a) -> m a
pluginInternals = (<$> pluginInternal)

pluginInternalL :: MonadRibo m => Lens' RibosomeInternal a -> m a
pluginInternalL = pluginInternals . Lens.view

pluginInternalPut' :: MonadRibo m => RibosomeInternal -> m ()
pluginInternalPut' s =
  pluginInternalModify (const s)

pluginInternalModify :: MonadRibo m => (RibosomeInternal -> RibosomeInternal) -> m ()
pluginInternalModify =
  void . pluginInternalModifyE . (Right .)

pluginInternalModifyL :: MonadRibo m => Lens' RibosomeInternal a -> (a -> a) -> m ()
pluginInternalModifyL l f =
  pluginInternalModify $ Lens.over l f

instance (MonadIO m, Nvim m) => MonadRibo (ReaderT (RiboConcState s) m) where
  pluginName =
    asks rsaName

  pluginInternal =
    liftIO =<< asks rsaInternalGet

  pluginInternalModifyE f = do
    m <- asks rsaInternalModify
    liftIO . m $ f

instance (MonadIO (t m), MonadTrans t, MonadRibo m) => MonadRibo (t m) where
  pluginName = lift pluginName
  pluginInternal = lift pluginInternal
  pluginInternalModifyE = lift . pluginInternalModifyE

instance MonadRibo (ConcNvimS e) where
  pluginName =
    asks $ Lens.view Ribosome.name

  pluginInternal = do
    tv <- asks $ Lens.view Ribosome.state
    Lens.view RibosomeState.internal <$> atomically (readTMVar tv)

  pluginInternalModifyE f = do
    tv <- asks $ Lens.view Ribosome.state
    liftIO $ modifyTv id tv (Lens.mapMOf RibosomeState.internal f)

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

inspectHeadE :: (MonadDeepState s s' m, MonadDeepError e e' m) => e' -> Lens' s' [a] -> m a
inspectHeadE err lens = do
  as <- gets $ Lens.view lens
  case as of
    (a : _) -> return a
    _ -> throwHoist err
