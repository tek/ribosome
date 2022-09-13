module Ribosome.Menu.Combinators where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import qualified Data.Trie as Trie
import Lens.Micro.Mtl (view)

import Ribosome.Menu.Class.MenuState (MenuState (Item, history, mode), entries, entryCount, query)
import Ribosome.Menu.Data.Entry (Entries, Entry, entriesLength)
import Ribosome.Menu.Data.State (MenuQuery (MenuQuery))
import Ribosome.Menu.Lens (use, (%=), (.=))

addHistory ::
  MenuState s =>
  Member (State s) r =>
  Sem r ()
addHistory = do
  MenuQuery oldQuery <- use query
  m <- use mode
  old <- use entries
  history m %= Trie.insert (encodeUtf8 oldQuery) old

updateEntries ::
  MenuState s =>
  Member (State s) r =>
  MenuQuery ->
  Entries (Item s) ->
  Sem r ()
updateEntries newQuery new = do
  entries .= new
  query .= newQuery
  entryCount .= entriesLength new

push ::
  MenuState s =>
  Member (State s) r =>
  MenuQuery ->
  Entries (Item s) ->
  Sem r ()
push newQuery new = do
  addHistory
  updateEntries newQuery new

numVisible ::
  MenuState s =>
  s ->
  Int
numVisible =
  sum . fmap length . view entries

sortEntries :: Entries i -> [Entry i]
sortEntries =
  concatMap (toList . snd) . IntMap.toDescList

sortEntriesText :: Entries i -> [Text]
sortEntriesText =
  fmap (view (#item . #text)) . sortEntries

sortedEntries ::
  MenuState s =>
  SimpleGetter s [Entry (Item s)]
sortedEntries =
  entries . to sortEntries

overEntries ::
  (Int -> Entry i -> Entry i) ->
  Entries i ->
  Entries i
overEntries f =
  snd . trans 0
  where
    trans =
      IntMap.mapAccumRWithKey \ index _ ->
        mapAccumL transSeq index
    transSeq i e =
      (i + 1, f i e)

foldEntries ::
  (a -> Int -> Entry i -> a) ->
  a ->
  Entries i ->
  a
foldEntries f a =
  snd . IntMap.foldr' (flip (foldl' transSeq)) (0, a)
  where
    transSeq (i, z) e =
      (i + 1, f z i e)

mapEntries :: (Int -> Entry i -> a) -> Entries i -> Seq a
mapEntries f =
  foldEntries folder Seq.empty
  where
    folder z i e =
      z |> f i e

partitionEntries ::
  (Int -> Entry i -> Either a b) ->
  Entries i ->
  ([a], [b])
partitionEntries f =
  foldEntries g ([], [])
  where
    g (l, r) i e =
      case f i e of
        Left a -> (a : l, r)
        Right b -> (l, b : r)

filterEntries ::
  (Int -> Entry i -> Bool) ->
  Entries i ->
  [Entry i]
filterEntries f =
  foldEntries g []
  where
    g z i e =
      if f i e then e : z else z

popEntries ::
  (Int -> Entry i -> Maybe a) ->
  Entries i ->
  ([a], Entries i)
popEntries f =
  first snd . trans (0, [])
  where
    trans =
      IntMap.mapAccumRWithKey \ z _ ->
        foldl' transSeq (z, Seq.empty)
    transSeq ((i, as), r) e =
      case f i e of
        Just a -> ((i + 1, a : as), r)
        Nothing -> ((i + 1, as), r |> e)
