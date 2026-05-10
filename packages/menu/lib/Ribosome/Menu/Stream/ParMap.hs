module Ribosome.Menu.Stream.ParMap where

import Control.Concurrent (getNumCapabilities)
import qualified Streamly.Data.Fold as Fold
import Streamly.Data.Stream (Stream)
import qualified Streamly.Data.Stream.Prelude as Stream

parConcatMap ::
  Int ->
  ([a] -> Stream IO b) ->
  Stream IO a ->
  Stream IO b
parConcatMap chunks f s =
  Stream.concatMap use (Stream.fromEffect getNumCapabilities)
  where
    use threads =
      Stream.parConcatMap (Stream.eager True . Stream.maxThreads (max 1 (threads - 1))) f chunked

    chunked = Stream.groupsOf chunks Fold.toList s

parMapChunks ::
  Int ->
  ([a] -> [b]) ->
  Stream IO a ->
  Stream IO b
parMapChunks chunks f =
  parConcatMap chunks (Stream.fromList . f)

parMap ::
  Int ->
  (a -> b) ->
  Stream IO a ->
  Stream IO b
parMap chunks f =
  parConcatMap chunks (Stream.fromList . fmap f)

parMapMaybe ::
  Int ->
  (a -> Maybe b) ->
  Stream IO a ->
  Stream IO b
parMapMaybe chunks f =
  parConcatMap chunks (Stream.fromList . mapMaybe f)

parMapChunksIO ::
  Int ->
  ([a] -> [b]) ->
  [a] ->
  IO [b]
parMapChunksIO chunks f =
  Stream.toList .
  parMapChunks chunks f .
  Stream.fromList

parMapIO ::
  Int ->
  (a -> b) ->
  [a] ->
  IO [b]
parMapIO chunks f =
  Stream.toList .
  parMap chunks f .
  Stream.fromList

parMapMaybeIO ::
  Int ->
  (a -> Maybe b) ->
  [a] ->
  IO [b]
parMapMaybeIO chunks f =
  Stream.toList .
  parMapMaybe chunks f .
  Stream.fromList
