{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ScratchSpec(
  htf_thisModulesTests
) where

import Test.Framework

import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Control.Monad.Ribo (RiboN)
import Ribosome.Data.ScratchOptions (ScratchOptions(ScratchOptions))
import Ribosome.Nvim.Api.IO (vimCommand)
import Ribosome.Scratch (showInScratch)
import Ribosome.Test.Unit (unitSpecDef)
import TestError (TestError)

target :: [Text]
target = ["line 1", "line 2"]

scratchSpec :: RiboN () TestError ()
scratchSpec = do
  _ <- showInScratch target (ScratchOptions False True False True (Just 0) [] [] "buffi")
  content <- currentBufferContent
  gassertEqual target content
  vimCommand "bdelete"

test_scratch :: IO ()
test_scratch =
  unitSpecDef () scratchSpec
