{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{- HLINT ignore -}

module Ribosome.Test.THTest where

import Data.Aeson (FromJSON)
-- import Data.Default (def)
import GHC.Generics (Generic)
import Language.Haskell.TH
import Neovim (Plugin(..))
-- import TestError (handleTestError)

-- import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Msgpack.Decode (MsgpackDecode)
import Ribosome.Msgpack.Encode (MsgpackEncode)
import Ribosome.Plugin
import Ribosome.Test.Run (UnitTest)

data Par =
  Par {
    parA :: Int,
    parB :: Int
  }
  deriving (Eq, Show, Generic, MsgpackDecode, MsgpackEncode, FromJSON)

handler :: Text -> m ()
handler =
  undefined

$(return [])

plugin' :: IO (Plugin (Ribosome Int))
plugin' = do
  -- ribo <- newRibosome ("test" :: Text) 1
  undefined
  -- return $ riboPlugin "test" ribo [$(rpcHandler (cmd []) 'handler)] [] handleTestError def

test_plug :: UnitTest
test_plug = do
  return ()
  -- _ <- plugin'
  -- traverse_ putStrLn $ lines $(stringE . pprint =<< rpcHandler (sync . cmd []) 'handler)
