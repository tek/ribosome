{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Nvim.Api.GenerateData where

import Language.Haskell.TH
import Neovim.Plugin.Classes (FunctionName(F))

import Ribosome.Nvim.Api.Generate (FunctionData(FunctionData), generateFromApi)
import Ribosome.Nvim.Api.RpcCall (AsyncRpcCall(..), RpcCall(..), SyncRpcCall(..))

dataSig :: [Type] -> Name -> Bool -> Q Dec
dataSig types name async = do
  returnType <- if async then [t|AsyncRpcCall|] else [t|SyncRpcCall|]
  sigD name . return . foldr (AppT . AppT ArrowT) returnType $ types

dataBody :: String -> Name -> Bool -> [Name] -> Q Dec
dataBody apiName name async params =
  funD name [clause (varP <$> params) (normalB $ appE syncCtor rpcCall) []]
  where
    rpcCall = [|RpcCall|] `appE` funcName `appE` listE (toObjVar <$> params)
    funcName = [|F . fromString|] `appE` (litE . stringL $ apiName)
    toObjVar v = [|toObject $(varE v)|]
    syncCtor = if async then [|AsyncRpcCall|] else [|SyncRpcCall|]

genCallData :: FunctionData -> Q [Dec]
genCallData (FunctionData apiName name async names types) = do
  sig <- dataSig types name async
  body <- dataBody apiName name async names
  return [sig, body]

generateData :: Q [Dec]
generateData =
  generateFromApi genCallData
