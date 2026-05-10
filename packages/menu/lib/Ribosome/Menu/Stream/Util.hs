module Ribosome.Menu.Stream.Util where

import Data.Maybe (fromJust)
import qualified Queue
import Streamly.Data.Stream (Stream)
import qualified Streamly.Data.Stream.Prelude as Stream

import Ribosome.Final (inFinal_)

takeUntilNothing ::
  Stream IO (Maybe a) ->
  Stream IO a
takeUntilNothing s =
  fromJust <$> Stream.takeWhile isJust s

repeatUntilNothing ::
  ∀ m a .
  Monad m =>
  m (Maybe a) ->
  Stream m a
repeatUntilNothing ma =
  spin
  where
    spin =
      Stream.concatMap next (Stream.fromEffect ma)

    next = \case
      Just a -> Stream.cons a spin
      Nothing -> Stream.nil

nothingTerminated ::
  Monad m =>
  Stream m a ->
  Stream m (Maybe a)
nothingTerminated s =
  Stream.append (Just <$> s) (Stream.fromPure Nothing)

queueStream ::
  Members [Queue a, Final IO] r =>
  Sem r (Stream IO a)
queueStream =
  inFinal_ \ lowerMaybe _ pureF ->
    pureF do
      takeUntilNothing (Stream.repeatM (join <$> lowerMaybe Queue.readMaybe))
