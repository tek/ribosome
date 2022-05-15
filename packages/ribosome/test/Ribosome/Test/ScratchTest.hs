module Ribosome.Test.ScratchTest where

import qualified Data.Map.Strict as Map

import Ribosome.Data.FloatOptions (FloatOptions (FloatOptions), FloatRelative (Cursor))
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.Scratch (Scratch)
import Ribosome.Data.ScratchOptions (ScratchOptions (ScratchOptions))
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Scratch (showInScratch)
-- import Ribosome.Test.Await (await)
-- import Ribosome.Test.Embed (integrationTestDef)

target :: [Text]
target = ["line 1", "line 2"]

name :: Text
name =
  "buffi"

makeScratch ::
  Members [Rpc !! RpcError, Rpc, AtomicState (Map Text Scratch), Reader PluginName, Log, Resource] r =>
  Sem r ()
makeScratch =
  void (showInScratch target options)
  where
    options =
      ScratchOptions False True False True True True False Nothing Nothing Nothing [] [] Nothing name

floatOptions :: FloatOptions
floatOptions =
  FloatOptions Cursor 30 2 1 1 True def Nothing def False True (Just def) Nothing

makeFloatScratch ::
  Members [Rpc !! RpcError, Rpc, AtomicState (Map Text Scratch), Reader PluginName, Log, Resource] r =>
  Sem r ()
makeFloatScratch =
  void (showInScratch target options)
  where
    options =
      ScratchOptions False True False True True True False (Just floatOptions) Nothing (Just 0) [] [] Nothing name

scratchCount ::
  Member (AtomicState (Map Text Scratch)) r =>
  Sem r Int
scratchCount =
  length . Map.toList <$> atomicGet

-- scratchPlugin :: IO (Plugin (Ribosome ()))
-- scratchPlugin = do
--   env <- newRibosome "test" ()
--   return $ riboPlugin "test" env funcs [] handleTestError def
--   where
--     funcs = [$(rpcHandlerDef 'makeScratch), $(rpcHandlerDef 'makeFloatScratch), $(rpcHandler sync 'scratchCount)]

-- scratchTest :: Text -> RiboTest ()
-- scratchTest fun = do
--   () <- vimCallFunction fun []
--   await ((1 :: Int) ===) scratches
--   await (target ===) currentBufferContent
--   nvimCommand "bdelete"
--   await (0 ===) scratches
--   where
--     scratches = vimCallFunction "ScratchCount" []

-- regularScratchTest :: RiboTest ()
-- regularScratchTest =
--   scratchTest "MakeScratch"

-- test_regularScratch :: UnitTest
-- test_regularScratch = do
--   plug <- liftIO scratchPlugin
--   integrationTestDef plug regularScratchTest

-- floatScratchTest :: RiboTest ()
-- floatScratchTest =
--   scratchTest "MakeFloatScratch"

-- test_floatScratch :: UnitTest
-- test_floatScratch = do
--   plug <- liftIO scratchPlugin
--   integrationTestDef plug floatScratchTest
