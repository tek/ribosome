module Ribosome.Host.TH.Api.GenerateData where

import Data.MessagePack (Object (ObjectExt))
import Language.Haskell.TH (
  Bang (Bang),
  DecQ,
  DecsQ,
  DerivClause (DerivClause),
  DerivStrategy (StockStrategy),
  Name,
  Q,
  SourceStrictness (SourceStrict),
  SourceUnpackedness (NoSourceUnpackedness),
  Type (AppT, ArrowT, ConT, VarT),
  clause,
  conE,
  conP,
  conT,
  dataD,
  funD,
  integerL,
  listE,
  litP,
  mkName,
  nameBase,
  normalB,
  normalC,
  sigD,
  varE,
  varP,
  )
import Prelude hiding (Type)

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (fromMsgpack))
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (toMsgpack))
import Ribosome.Host.Class.Msgpack.Util (illegalType)
import Ribosome.Host.Data.ApiInfo (ExtTypeMeta (ExtTypeMeta))
import Ribosome.Host.Data.ApiType (ApiPrim (Object), ApiType (Prim))
import Ribosome.Host.Data.Request (Request (Request), RpcMethod (RpcMethod))
import Ribosome.Host.TH.Api.Generate (MethodSpec (MethodSpec), generateFromApi, reifyApiType)

effectiveType :: ApiType -> Q Type
effectiveType = \case
  Prim Object ->
    pure (VarT (mkName "a"))
  a ->
    reifyApiType a

dataSig :: [Type] -> Name -> ApiType -> DecQ
dataSig types name returnType = do
  t <- [t|Request $(effectiveType returnType)|]
  sigD name (pure (foldr (AppT . AppT ArrowT) t types))

dataBody :: String -> Name -> [Name] -> DecQ
dataBody apiName name params =
  funD name [clause (varP <$> params) (normalB rpcCall) []]
  where
    rpcCall =
      [|Request (RpcMethod apiName) $(listE (toObjVar <$> params))|]
    toObjVar v =
      [|toMsgpack $(varE v)|]

genRequest :: MethodSpec -> DecsQ
genRequest (MethodSpec apiName name paramNames paramTypes returnType) = do
  sig <- dataSig paramTypes name returnType
  body <- dataBody apiName name paramNames
  pure [sig, body]

extData :: Name -> DecQ
extData name =
  dataD (pure []) name [] Nothing [con] (deriv ["Eq", "Show"])
  where
    con =
      normalC name [(Bang NoSourceUnpackedness SourceStrict,) <$> [t|ByteString|]]
    deriv cls =
      [pure (DerivClause (Just StockStrategy) (ConT . mkName <$> cls))]

decodeInstance :: Name -> Int64 -> DecsQ
decodeInstance name number =
  [d|
  instance MsgpackDecode $(conT name) where
    fromMsgpack = \case
      ObjectExt $(litP (integerL (fromIntegral number))) bytes ->
        pure ($(conE name) bytes)
      o ->
        illegalType (toText (nameBase name)) o
  |]

encodeInstance :: Name -> Int64 -> DecsQ
encodeInstance name number =
  [d|
  instance MsgpackEncode $(conT name) where
    toMsgpack $(conP name [varP (mkName "bytes")]) =
      ObjectExt number bytes
  |]

extDataCodec :: Name -> Int64 -> DecsQ
extDataCodec name number =
  mappend <$> decodeInstance name number <*> encodeInstance name number

genExtTypes :: Name -> ExtTypeMeta -> DecsQ
genExtTypes name (ExtTypeMeta number _) =
  (:) <$> extData name <*> extDataCodec name number

generateData :: DecsQ
generateData =
  generateFromApi genRequest (Just genExtTypes)
