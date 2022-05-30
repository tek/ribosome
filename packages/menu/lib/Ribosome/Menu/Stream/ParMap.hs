module Ribosome.Menu.Stream.ParMap where

import Control.Concurrent (getNumCapabilities)
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.IsStream.Exception as Stream
import qualified Streamly.Prelude as Stream
import Streamly.Prelude (IsStream)

parConcatMap ::
  IsStream t =>
  Int ->
  ([a] -> t IO b) ->
  t IO a ->
  t IO b
parConcatMap chunks f s =
  Stream.bracket_ getNumCapabilities (const unit) \ threads ->
    Stream.maxThreads threads $
    Stream.fromParallel $
    Stream.concatMapWith Stream.parallel (Stream.adapt . f) $
    Stream.adapt $
    Stream.chunksOf chunks Fold.toList s

parMap ::
  IsStream t =>
  Int ->
  (a -> b) ->
  t IO a ->
  t IO b
parMap chunks f =
  parConcatMap chunks (Stream.fromList . fmap f)

parMapIO ::
  Int ->
  (a -> b) ->
  [a] ->
  IO [b]
parMapIO chunks f =
  Stream.toList .
  parMap chunks f .
  Stream.fromList
