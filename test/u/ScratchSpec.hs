{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ScratchSpec(
  htf_thisModulesTests
) where

import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Neovim (Neovim)
import Test.Framework

import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Control.Monad.Ribo (ConcNvimS, RiboE)
import Ribosome.Control.Ribosome (Ribosome)
import Ribosome.Data.ScratchOptions (ScratchOptions(ScratchOptions))
import Ribosome.Nvim.Api.RpcCall (RpcError)
import Ribosome.Scratch (showInScratch)
import Ribosome.Test.Unit (unitSpecDef)

target :: [String]
target = ["line 1", "line 2"]

scratchSpec :: RiboE () RpcError (Neovim (Ribosome ())) ()
scratchSpec = do
  _ <- showInScratch target (ScratchOptions False True (Just 0) False "buffi")
  content <- currentBufferContent
  gassertEqual target content

test_scratch :: IO ()
test_scratch =
  unitSpecDef () scratchSpec
