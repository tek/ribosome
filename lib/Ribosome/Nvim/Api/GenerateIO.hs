{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Nvim.Api.GenerateIO where

import qualified Control.Lens as Lens (review)
import Control.Monad.Error.Class (MonadError, liftEither)
import Data.Either.Combinators (mapLeft)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Ribosome.Control.Monad.Ribo (Nvim(call))
import Ribosome.Msgpack.Decode (MsgpackDecode)
import Ribosome.Nvim.Api.Generate (FunctionData(FunctionData), generateFromApi)
import Ribosome.Nvim.Api.RpcCall (AsRpcError(_RpcError))

rpcModule :: Module
rpcModule =
  Module (mkPkgName "Ribosome.Nvim.Api") (mkModName "Data")

ioSig :: Name -> [Type] -> Q Dec
ioSig name types = do
  nvimConstraint <- [t|Nvim $(vtq "m")|]
  decodeConstraint <- [t|MsgpackDecode $(vtq "a")|]
  monadErrorConstraint <- [t|MonadError $(vtq "e") $(vtq "m")|]
  asRpcErrorConstraint <- [t|AsRpcError $(vtq "e")|]
  let
    returnType = AppT (vt "m") (vt "a")
    params = foldr (AppT . AppT ArrowT) returnType types
    constraints = [nvimConstraint, decodeConstraint, monadErrorConstraint, asRpcErrorConstraint]
  sigD name $ return (ForallT [] constraints params)
  where
    vt = VarT . mkName
    vtq = varT . mkName

ioBody :: Name -> Bool -> [Name] -> Q Dec
ioBody name _ names =
  funD name [clause (varP <$> names) (normalB body) []]
  where
    responseName = mkName "response"
    callPat = varP responseName
    callExp = appE [|call|] args
    checkExp = appE [|liftEither . mapLeft (Lens.review _RpcError)|] (varE responseName)
    args = foldl appE (varE $ mkName $ "RpcData." ++ show name) (varE <$> names)
    body = doE [bindS callPat callExp, noBindS checkExp]

genIO :: FunctionData -> Q [Dec]
genIO (FunctionData _ name async names types) = do
  sig <- ioSig name types
  body <- ioBody name async names
  return [sig, body]

generateIO :: Q [Dec]
generateIO =
  generateFromApi genIO
