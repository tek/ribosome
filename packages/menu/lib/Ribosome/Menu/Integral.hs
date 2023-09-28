module Ribosome.Menu.Integral where

subClamp ::
  ∀ a b .
  Ord b =>
  Num b =>
  Integral a =>
  b ->
  a ->
  b
subClamp a (fromIntegral -> b) | b < a = a - b
                               | otherwise = 0
