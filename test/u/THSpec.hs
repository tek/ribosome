{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module THSpec(
  htf_thisModulesTests,
) where

import Data.Aeson (FromJSON)
-- import Data.Foldable (traverse_)
import GHC.Generics (Generic)
-- import Language.Haskell.TH
import Neovim (Plugin(..))
import Ribosome.Data.Mapping (MappingError)
import Test.Framework

import Ribosome.Control.Monad.Ribo (ConcNvimS, RiboE)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Msgpack.Decode (MsgpackDecode)
import Ribosome.Msgpack.Encode (MsgpackEncode)
import Ribosome.Plugin
import TestError (handleTestError)

data Par =
  Par {
    parA :: Int,
    parB :: Int
  }
  deriving (Eq, Show, Generic, MsgpackDecode, MsgpackEncode, FromJSON)

handler :: Monad m => Int -> Text -> Par -> m ()
handler =
  undefined

type R = RiboE Int MappingError (ConcNvimS Int)

$(return [])

plugin' :: IO (Plugin (Ribosome Int))
plugin' = do
  ribo <- newRibosome ("test" :: Text) 1
  return $ nvimPlugin "test" ribo [$(rpcHandler (cmd []) 'handler)] handleTestError

test_plug :: IO ()
test_plug = do
  _ <- plugin'
  return ()
  -- traverse_ putStrLn $ lines $(stringE . pprint =<< rpcHandler (cmd []) 'handler)
