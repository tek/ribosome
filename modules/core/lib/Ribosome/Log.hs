module Ribosome.Log where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as ByteString (toString)
import Data.Foldable (traverse_)
import Data.Text (Text)
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
prefixed prefix a = liftIO $ putStrLn $ prefix <> ": " <> show a

logR ::
  Loggable a =>
  MonadRibo m =>
  MonadIO m =>
  Priority ->
  a ->
  m ()
logR prio message = do
  n <- pluginName
  logAs prio n message

debug ::
  Loggable a =>
  MonadRibo m =>
  MonadIO m =>
  a ->
  m ()
debug =
  logR DEBUG

info ::
  Loggable a =>
  MonadRibo m =>
  MonadIO m =>
  a ->
  m ()
info =
  logR INFO

err ::
  Loggable a =>
  MonadRibo m =>
  MonadIO m =>
  a ->
  m ()
err =
  logR ERROR

debugShow ::
  Show a =>
  MonadRibo m =>
  MonadIO m =>
  a ->
  m ()
debugShow =
  debug @Text . show

infoShow ::
  Show a =>
  MonadRibo m =>
  MonadIO m =>
  a ->
  m ()
infoShow =
  info @Text . show

errShow ::
  Show a =>
  MonadRibo m =>
  MonadIO m =>
  a ->
  m ()
errShow =
  err @Text . show
