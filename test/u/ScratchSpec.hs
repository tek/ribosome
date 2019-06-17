{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ScratchSpec (htf_thisModulesTests) where

import Data.Default (def)
import qualified Data.Map.Strict as Map (toList)
import Neovim (Plugin(..))
import Test.Framework

import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE, pluginInternalL)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import qualified Ribosome.Control.Ribosome as Ribosome (scratch)
import Ribosome.Data.FloatOptions (FloatOptions(FloatOptions), FloatRelative(Cursor))
import Ribosome.Data.ScratchOptions (ScratchOptions(ScratchOptions))
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (vimCallFunction, vimCommand)
import Ribosome.Plugin (riboPlugin, rpcHandler, rpcHandlerDef, sync)
import Ribosome.Scratch (showInScratch)
import Ribosome.Test.Await (await)
import Ribosome.Test.Embed (integrationSpecDef)
import Ribosome.Test.Tmux (tmuxIntegrationSpecDef)
import TestError (RiboT, handleTestError)

target :: [Text]
target = ["line 1", "line 2"]

name :: Text
name =
  "buffi"

makeScratch ::
  MonadDeepError e DecodeError m =>
  MonadRibo m =>
  NvimE e m =>
  m ()
makeScratch =
  void $ showInScratch target (ScratchOptions False True False True True True Nothing Nothing (Just 0) [] [] name)

floatOptions :: FloatOptions
floatOptions =
  FloatOptions Cursor 30 2 1 1 True def

makeFloatScratch ::
  MonadDeepError e DecodeError m =>
  MonadRibo m =>
  NvimE e m =>
  m ()
makeFloatScratch =
  void $ showInScratch target options
  where
    options =
      ScratchOptions False True False True True True (Just floatOptions) Nothing (Just 0) [] [] name

scratchCount ::
  MonadRibo m =>
  m Int
scratchCount =
  length . Map.toList <$> pluginInternalL Ribosome.scratch

$(return [])

scratchPlugin :: IO (Plugin (Ribosome ()))
scratchPlugin = do
  env <- newRibosome "test" ()
  return $ riboPlugin "test" env funcs [] handleTestError def
  where
    funcs = [$(rpcHandlerDef 'makeScratch), $(rpcHandlerDef 'makeFloatScratch), $(rpcHandler sync 'scratchCount)]

scratchSpec :: Text -> RiboT ()
scratchSpec fun = do
  () <- vimCallFunction fun []
  await (gassertEqual (1 :: Int)) scratches
  await (gassertEqual target) currentBufferContent
  vimCommand "bdelete"
  await (gassertEqual 0) scratches
  where
    scratches = vimCallFunction "ScratchCount" []

regularScratchSpec :: RiboT ()
regularScratchSpec =
  scratchSpec "MakeScratch"

test_scratch :: IO ()
test_scratch = do
  plug <- scratchPlugin
  integrationSpecDef plug regularScratchSpec

floatScratchSpec :: RiboT ()
floatScratchSpec =
  scratchSpec "MakeFloatScratch"

test_floatScratch :: IO ()
test_floatScratch = do
  plug <- scratchPlugin
  integrationSpecDef plug floatScratchSpec
