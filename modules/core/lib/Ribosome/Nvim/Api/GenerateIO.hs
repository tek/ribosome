{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Nvim.Api.GenerateIO where

import Control.Monad ((<=<))
import Control.Monad.DeepError (MonadDeepError, hoistEither)
import Data.Maybe (maybeToList)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Neovim.API.Parser (NeovimType(SimpleType))

import Ribosome.Control.Monad.Ribo (Nvim(call))
import Ribosome.Msgpack.Decode (MsgpackDecode)
import Ribosome.Nvim.Api.Generate (FunctionData(FunctionData), generateFromApi, haskellType)
import Ribosome.Nvim.Api.RpcCall (RpcError)

rpcModule :: Module
rpcModule =
  Module (mkPkgName "Ribosome.Nvim.Api") (mkModName "Data")

msgpackDecodeConstraint :: NeovimType -> Q (Maybe Type)
msgpackDecodeConstraint (SimpleType "Object") =
  Just <$> [t|MsgpackDecode $(varT $ mkName "a")|]
msgpackDecodeConstraint _ =
  return Nothing

newT :: String -> TypeQ
newT =
  varT <=< newName

ioReturnType :: NeovimType -> Q Type
ioReturnType (SimpleType "Object") =
  return (VarT $ mkName "a")
ioReturnType a =
  haskellType a

analyzeReturnType :: NeovimType -> Q (Type, Maybe Type)
analyzeReturnType tpe = do
  rt <- ioReturnType tpe
  constraint <- msgpackDecodeConstraint tpe
  return (rt, constraint)

ioSig :: Name -> [Type] -> NeovimType -> DecQ
ioSig name types returnType = do
  mType <- newT "m"
  nvimConstraint <- [t|Nvim $(pure mType)|]
  (retType, decodeConstraint) <- analyzeReturnType returnType
  monadErrorConstraint <- [t|MonadDeepError $(newT "e") RpcError $(pure mType)|]
  let
    params = foldr (AppT . AppT ArrowT) (AppT mType retType) types
    constraints = [nvimConstraint, monadErrorConstraint] ++ maybeToList decodeConstraint
  sigD name $ return (ForallT [] constraints params)

ioBody :: Name -> Bool -> [Name] -> DecQ
ioBody name _ names =
  funD name [clause (varP <$> names) (normalB body) []]
  where
    responseName = mkName "response"
    callPat = varP responseName
    callExp = appE [|call|] args
    checkExp = appE [|hoistEither|] (varE responseName)
    args = foldl appE (varE $ mkName $ "RpcData." ++ show name) (varE <$> names)
    body = doE [bindS callPat callExp, noBindS checkExp]

genIO :: FunctionData -> Q [Dec]
genIO (FunctionData _ name async names types returnType) = do
  sig <- ioSig name types returnType
  body <- ioBody name async names
  return [sig, body]

generateIO :: Q [Dec]
generateIO =
  generateFromApi genIO (const . const . return $ [])
