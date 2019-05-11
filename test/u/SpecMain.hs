{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework
import Test.Framework.BlackBoxTest ()

import {-@ HTF_TESTS @-} AutocmdSpec
import {-@ HTF_TESTS @-} MappingSpec
import {-@ HTF_TESTS @-} MenuSpec
import {-@ HTF_TESTS @-} MsgpackSpec
import {-@ HTF_TESTS @-} ScratchSpec
import {-@ HTF_TESTS @-} SettingSpec
import {-@ HTF_TESTS @-} SyntaxSpec
import {-@ HTF_TESTS @-} THSpec
import {-@ HTF_TESTS @-} WatcherSpec

main :: IO ()
main = htfMain htf_importedTests
