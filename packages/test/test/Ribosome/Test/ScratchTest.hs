module Ribosome.Test.ScratchTest where

import qualified Data.Map.Strict as Map (toList)
import Hedgehog ((===))
import Neovim (Plugin (..))
import TestError (RiboTest, handleTestError)

import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE, pluginInternalL)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import qualified Ribosome.Control.Ribosome as Ribosome (scratch)
import Ribosome.Data.FloatOptions (FloatOptions (FloatOptions), FloatRelative (Cursor))
import Ribosome.Data.ScratchOptions (ScratchOptions (ScratchOptions))
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (vimCallFunction, vimCommand)
import Ribosome.Plugin (riboPlugin, rpcHandler, rpcHandlerDef, sync)
import Ribosome.Scratch (showInScratch)
import Ribosome.Test.Await (await)
import Ribosome.Test.Embed (integrationTestDef)
import Ribosome.Test.Run (UnitTest)

target :: [Text]
target = ["line 1", "line 2"]

name :: Text
name =
  "buffi"

makeScratch ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  m ()
makeScratch =
  void $ showInScratch target (ScratchOptions False True False True True True False Nothing Nothing Nothing [] [] Nothing name)

floatOptions :: FloatOptions
floatOptions =
  FloatOptions Cursor 30 2 1 1 True def Nothing def False True (Just def) Nothing

makeFloatScratch ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  m ()
makeFloatScratch =
  void $ showInScratch target options
  where
    options =
      ScratchOptions False True False True True True False (Just floatOptions) Nothing (Just 0) [] [] Nothing name

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

scratchTest :: Text -> RiboTest ()
scratchTest fun = do
  () <- vimCallFunction fun []
  await ((1 :: Int) ===) scratches
  await (target ===) currentBufferContent
  vimCommand "bdelete"
  await (0 ===) scratches
  where
    scratches = vimCallFunction "ScratchCount" []

regularScratchTest :: RiboTest ()
regularScratchTest =
  scratchTest "MakeScratch"

test_regularScratch :: UnitTest
test_regularScratch = do
  plug <- liftIO scratchPlugin
  integrationTestDef plug regularScratchTest

floatScratchTest :: RiboTest ()
floatScratchTest =
  scratchTest "MakeFloatScratch"

test_floatScratch :: UnitTest
test_floatScratch = do
  plug <- liftIO scratchPlugin
  integrationTestDef plug floatScratchTest
