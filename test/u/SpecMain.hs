{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework
import Test.Framework.BlackBoxTest ()
import {-@ HTF_TESTS @-} MsgpackSpec
import {-@ HTF_TESTS @-} ScratchSpec
import {-@ HTF_TESTS @-} SettingSpec
import {-@ HTF_TESTS @-} SyntaxSpec
import {-@ HTF_TESTS @-} THSpec

main :: IO ()
main = htfMain htf_importedTests
