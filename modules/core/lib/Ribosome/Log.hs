module Ribosome.Log(
  debug,
  info,
  err,
  p,
  prefixed,
  debugR,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Neovim.Log (debugM, errorM, infoM)

import Ribosome.Control.Monad.Ribo (MonadRibo, pluginName)

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

debugR :: (MonadRibo m, MonadIO m) => String -> m ()
debugR a = do
  n <- pluginName
  liftIO $ debugM n a
