module Ribosome.Nvim.Api.GenerateData where

import Data.MessagePack (Object(ObjectExt))
import Language.Haskell.TH
import Neovim.Plugin.Classes (FunctionName(F))

import Ribosome.Msgpack.Decode (MsgpackDecode)
import Ribosome.Msgpack.Encode (MsgpackEncode)
import Ribosome.Msgpack.Util (illegalType)
import Ribosome.Nvim.Api.Generate (FunctionData(FunctionData), generateFromApi)
import Ribosome.Nvim.Api.RpcCall (AsyncRpcCall(..), RpcCall(..), SyncRpcCall(..))

dataSig :: [Type] -> Name -> Bool -> DecQ
dataSig types name async = do
  returnType <- if async then [t|AsyncRpcCall|] else [t|SyncRpcCall|]
  sigD name . return . foldr (AppT . AppT ArrowT) returnType $ types

dataBody :: String -> Name -> Bool -> [Name] -> DecQ
dataBody apiName name async params =
  funD name [clause (varP <$> params) (normalB $ appE syncCtor rpcCall) []]
  where
    rpcCall = [|RpcCall|] `appE` funcName `appE` listE (toObjVar <$> params)
    funcName = [|F . fromString|] `appE` (litE . stringL $ apiName)
    toObjVar v = [|toMsgpack $(varE v)|]
    syncCtor = if async then [|AsyncRpcCall|] else [|SyncRpcCall|]

genCallData :: FunctionData -> DecsQ
genCallData (FunctionData apiName name async names types _) = do
  sig <- dataSig types name async
  body <- dataBody apiName name async names
  return [sig, body]

extData :: Name -> DecQ
extData name =
  dataD (return []) name [] Nothing [ctor] (deriv ["Eq", "Show"])
  where
    ctor = normalC name [(Bang NoSourceUnpackedness SourceStrict,) <$> [t|ByteString|]]
    deriv = return . return . DerivClause Nothing . (ConT . mkName <$>)

decClause :: Name -> Int64 -> ClauseQ
decClause name number = do
  bytesVar <- newName "bytes"
  clause [pat bytesVar] (decBody bytesVar) []
  where
    pat bytesVar = conP (mkName "ObjectExt") [(litP . integerL . fromIntegral) number, varP bytesVar]
    decBody bytesVar = (normalB [|return $ $(conE name) $(varE bytesVar)|])

decErrorClause :: Name -> ClauseQ
decErrorClause name = do
  objectVar <- newName "object"
  clause [varP objectVar] (decBody objectVar) []
  where
    nameString = nameBase name
    decBody objectVar = normalB [|illegalType nameString $(varE objectVar)|]

encClause :: Name -> Int64 -> ClauseQ
encClause name number = do
  bytesVar <- newName "bytes"
  clause [conP name [varP bytesVar]] (encBody bytesVar) []
  where
    encBody bytesVar = normalB [|ObjectExt $((litE . integerL . fromIntegral) number) $(varE bytesVar)|]

extDataCodec :: Name -> Int64 -> DecsQ
extDataCodec name number = do
  dec <- inst [t|MsgpackDecode|] [method "fromMsgpack" [decClause name number, decErrorClause name]]
  enc <- inst [t|MsgpackEncode|] [method "toMsgpack" [encClause name number]]
  return [dec, enc]
  where
    inst t = instanceD (return []) (tpe t)
    tpe = (`appT` conT name)
    method methodName clauses = funD (mkName methodName) clauses

genExtTypes :: Name -> Int64 -> DecsQ
genExtTypes name number = do
  dat <- extData name
  codec <- extDataCodec name number
  return (dat : codec)

generateData :: DecsQ
generateData =
  generateFromApi genCallData genExtTypes
