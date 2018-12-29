{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import {-@ HTF_TESTS @-} ScratchSpec
import Test.Framework
import Test.Framework.BlackBoxTest ()

main :: IO ()
main = htfMain htf_importedTests
