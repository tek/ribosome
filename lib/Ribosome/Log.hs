module Ribosome.Log(
  debug,
  info,
  p,
  prefixed,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Neovim.Log (debugM, infoM)

debug :: (MonadIO m, Show a) => String -> a -> m ()
debug name message = liftIO $ debugM name $ show message

info :: (MonadIO m, Show a) => String -> a -> m ()
info name message = liftIO $ infoM name $ show message

p :: (MonadIO m, Show a) => a -> m ()
p = liftIO . print

prefixed :: (MonadIO m, Show a) => String -> a -> m ()
prefixed prefix a = liftIO $ putStrLn $ prefix ++ ": " ++ show a
