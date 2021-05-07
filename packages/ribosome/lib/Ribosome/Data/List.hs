module Ribosome.Data.List where

import qualified Data.Set as Set (difference, fromList, toList)

mapSelectors :: (a -> a) -> [Int] -> [a] -> [a]
mapSelectors f indexes =
  reverse . go 0 (sort indexes) []
  where
    go current (i : is) result (a : asTail) | i == current =
      go (current + 1) is (f a : result) asTail
    go current is result (a : asTail) =
      go (current + 1) is (a : result) asTail
    go _ _ result _ =
      result

indexesComplement :: Int -> [Int] -> [Int]
indexesComplement total =
 Set.toList . Set.difference allIndexes . Set.fromList
  where
    allIndexes =
      Set.fromList [0..total - 1]
