{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ScratchSpec(
  htf_thisModulesTests
) where

import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Control.Ribo (Ribo)
import Ribosome.Data.ScratchOptions (ScratchOptions(ScratchOptions))
import Ribosome.Scratch (showInScratch)
import Ribosome.Test.Unit (unitSpec)
import Test.Framework

target :: [String]
target = ["line 1", "line 2"]

scratchSpec :: Ribo e ()
scratchSpec = do
  _ <- showInScratch target (ScratchOptions False True (Just 0) False "buffi")
  content <- currentBufferContent
  liftIO $ assertEqual target content

test_scratch :: IO ()
test_scratch =
  unitSpec def () scratchSpec
