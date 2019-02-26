module Ribosome.Log(
  debug,
  info,
  err,
  p,
  prefixed,
  debugR,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Neovim (ask)
import Neovim.Log (debugM, infoM, errorM)

import Ribosome.Control.Ribo (Ribo)
import qualified Ribosome.Control.Ribosome as R (name)

debug :: (MonadIO m, Show a) => String -> a -> m ()
debug name message = liftIO $ debugM name $ show message

info :: (MonadIO m, Show a) => String -> a -> m ()
info name message = liftIO $ infoM name $ show message

err :: (MonadIO m, Show a) => String -> a -> m ()
err name message = liftIO $ errorM name $ show message

p :: (MonadIO m, Show a) => a -> m ()
p = liftIO . print

prefixed :: (MonadIO m, Show a) => String -> a -> m ()
prefixed prefix a = liftIO $ putStrLn $ prefix ++ ": " ++ show a

debugR :: String -> Ribo e ()
debugR a = do
  pluginName <- R.name <$> ask
  liftIO $ debugM pluginName a
