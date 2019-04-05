{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoOverloadedStrings #-}

module THSpec(
  htf_thisModulesTests,
) where

-- import Control.Monad.Error.Class (MonadError)
-- import Control.Monad.Trans.Class (lift)
-- import Control.Monad.Trans.Except (ExceptT)
-- import Data.Foldable (traverse_)
-- import Language.Haskell.TH
-- import Neovim (Plugin(..))
-- import Neovim.Plugin.Classes
import Test.Framework

-- import Ribosome.Control.Monad.Ribo (ConcNvimS, Ribo, runRib)
-- import Ribosome.Control.Ribosome (Ribosome, newRibosome)
-- import Ribosome.Msgpack.Decode (MsgpackDecode)
-- import Ribosome.Msgpack.Encode (MsgpackEncode)
-- import Ribosome.Nvim.Api.GenerateData
-- import Ribosome.Nvim.Api.GenerateIO
-- import Ribosome.Plugin
-- import Ribosome.Plugin.TH

-- newtype Par =
--   Par Int
--   deriving (Eq, Show, MsgpackDecode, MsgpackEncode)

-- handler :: Monad m => Int -> String -> Par -> m ()
-- handler =
--   undefined

-- type R = Ribo Int (ConcNvimS Int)

-- instance RpcHandler String (Ribosome Int) R where
--   native = lift . runRib

-- handleError :: String -> R ()
-- handleError _ =
--   return ()

-- $(return [])

-- plugin' :: IO (Plugin (Ribosome Int))
-- plugin' = do
--   ribo <- newRibosome "test" 1
--   return $ nvimPlugin @String @(Ribosome Int) @R ribo [$(rpcHandler (cmd []) 'handler)] handleError

-- test_plug :: IO ()
-- test_plug =
--   traverse_ putStrLn $ lines $(stringE . pprint =<< rpcHandler (cmd []) 'handler)
