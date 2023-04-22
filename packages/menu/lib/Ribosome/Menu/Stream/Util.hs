module Ribosome.Menu.Stream.Util where

import Data.Maybe (fromJust)
import qualified Queue
import qualified Streamly.Prelude as Stream
import Streamly.Prelude (IsStream)

import Ribosome.Final (inFinal_)

takeUntilNothing ::
  Monad m =>
  IsStream t =>
  Functor (t m) =>
  t m (Maybe a) ->
  t m a
takeUntilNothing s =
  fromJust <$> Stream.takeWhile isJust s

repeatUntilNothing :: 
  ∀ t m a .
  Monad m =>
  Monad (t m) =>
  IsStream t =>
  m (Maybe a) ->
  t m a
repeatUntilNothing ma =
  spin
  where
    spin =
      Stream.fromEffect ma >>= \case
        Just a -> Stream.cons a spin
        Nothing -> Stream.nil

nothingTerminated ::
  Monad m =>
  IsStream t =>
  t m a ->
  t m (Maybe a)
nothingTerminated s =
  Stream.serial (Just <$> s) (Stream.fromPure Nothing)

queueStream ::
  IsStream t =>
  Functor (t IO) =>
  Members [Queue a, Final IO] r =>
  Sem r (t IO a)
queueStream =
  inFinal_ \ lowerMaybe _ pureF ->
    pureF do
      takeUntilNothing (Stream.repeatM (join <$> lowerMaybe Queue.readMaybe))
