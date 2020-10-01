module Ribosome.Log where

import qualified Data.ByteString.UTF8 as ByteString (toString)
import qualified Data.Text as Text (unpack)
import System.Log.Logger (Priority(DEBUG, ERROR, INFO), logM)

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
  Priority ->
  Text ->
  a ->
  m ()
logAs prio name = liftIO . traverse_ (logM (toString name) prio) . logLines

debugAs ::
  Loggable a =>
  MonadIO m =>
  Text ->
  a ->
  m ()
debugAs =
  logAs DEBUG

infoAs ::
  Loggable a =>
  MonadIO m =>
  Text ->
  a ->
  m ()
infoAs =
  logAs INFO

errAs ::
  Loggable a =>
  MonadIO m =>
  Text ->
  a ->
  m ()
errAs =
  logAs ERROR

prefixed :: (MonadIO m, Show a) => Text -> a -> m ()
prefixed prefix a = liftIO . putStrLn . toString $ prefix <> ": " <> show a

logR ::
  Loggable a =>
  MonadRibo m =>
  Priority ->
  a ->
  m ()
logR prio message = do
  n <- pluginName
  logAs prio n message

debug ::
  Loggable a =>
  MonadRibo m =>
  a ->
  m ()
debug =
  logR DEBUG

logDebug ::
  Loggable a =>
  MonadRibo m =>
  a ->
  m ()
logDebug = debug

showDebug ::
  Show a =>
  MonadRibo m =>
  Text ->
  a ->
  m ()
showDebug prefix a =
  logDebug @Text (prefix <> " " <> show a)

showDebugM ::
  Show a =>
  MonadRibo m =>
  Text ->
  m a ->
  m a
showDebugM prefix ma = do
  a <- ma
  logDebug @Text (prefix <> " " <> show a)
  return a

info ::
  Loggable a =>
  MonadRibo m =>
  a ->
  m ()
info =
  logR INFO

logInfo ::
  Loggable a =>
  MonadRibo m =>
  a ->
  m ()
logInfo = info

err ::
  Loggable a =>
  MonadRibo m =>
  a ->
  m ()
err =
  logR ERROR

logError ::
  Loggable a =>
  MonadRibo m =>
  a ->
  m ()
logError = err

showError ::
  Show a =>
  MonadRibo m =>
  Text ->
  a ->
  m ()
showError prefix a =
  logError @Text (prefix <> " " <> show a)
