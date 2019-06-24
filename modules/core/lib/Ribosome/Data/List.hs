module Ribosome.Data.List where

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
