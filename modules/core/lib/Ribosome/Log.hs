module Ribosome.Log where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as ByteString (toString)
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import Neovim.Log (debugM, errorM, infoM)

import Ribosome.Control.Monad.Ribo (MonadRibo, pluginName)

class Loggable a where
  logLines :: a -> [String]

instance {-# OVERLAPPABLE #-} Loggable a => Loggable [a] where
  logLines =
    (>>= logLines)

instance {-# OVERLAPPING #-} Loggable String where
  logLines = pure

instance Loggable ByteString where
  logLines = pure . ByteString.toString

instance Loggable Text where
  logLines = pure . Text.unpack

logAs ::
  Loggable a =>
  MonadIO m =>
  (String -> String -> IO ()) ->
  String ->
  a ->
  m ()
logAs logger name = liftIO . traverse_ (logger name) . logLines

debugAs ::
  Loggable a =>
  MonadIO m =>
  String ->
  a ->
  m ()
debugAs =
  logAs debugM

infoAs ::
  Loggable a =>
  MonadIO m =>
  String ->
  a ->
  m ()
infoAs =
  logAs infoM

errAs ::
  Loggable a =>
  MonadIO m =>
  String ->
  a ->
  m ()
errAs =
  logAs errorM

p :: (MonadIO m, Show a) => a -> m ()
p = liftIO . print

prefixed :: (MonadIO m, Show a) => String -> a -> m ()
prefixed prefix a = liftIO $ putStrLn $ prefix ++ ": " ++ show a

logR ::
  Loggable a =>
  MonadRibo m =>
  MonadIO m =>
  (String -> String -> IO ()) ->
  a ->
  m ()
logR logger message = do
  n <- pluginName
  logAs logger n message

debug ::
  Loggable a =>
  MonadRibo m =>
  MonadIO m =>
  a ->
  m ()
debug =
  logR debugM

info ::
  Loggable a =>
  MonadRibo m =>
  MonadIO m =>
  a ->
  m ()
info =
  logR infoM

err ::
  Loggable a =>
  MonadRibo m =>
  MonadIO m =>
  a ->
  m ()
err =
  logR errorM
