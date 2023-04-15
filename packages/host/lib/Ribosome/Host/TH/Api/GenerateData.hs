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
  Specificity (SpecifiedSpec),
  TyVarBndr (KindedTV),
  Type (AppT, ArrowT, ConT, ForallT, StarT, VarT),
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
  newName,
  normalB,
  normalC,
  sigD,
  varE,
  varP,
  varT,
  )
import Prelude hiding (Type)

import Ribosome.Host.Class.MonadRpc (MonadRpc, rpcRequest)
import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (fromMsgpack))
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (toMsgpack))
import Ribosome.Host.Class.Msgpack.Error (decodeIncompatible)
import Ribosome.Host.Data.ApiInfo (ExtTypeMeta (ExtTypeMeta))
import Ribosome.Host.Data.ApiType (ApiType, pattern PolyType)
import Ribosome.Host.Data.Request (Request (Request), RpcMethod (RpcMethod))
import Ribosome.Host.TH.Api.Generate (MethodSpec (MethodSpec), generateFromApi, reifyApiType)
import Ribosome.Host.TH.Api.Param (Param (Param), paramName)

msgpackDecodeConstraint :: ApiType -> Q (Maybe Type)
msgpackDecodeConstraint = \case
  PolyType ->
    Just <$> [t|MsgpackDecode $(varT (mkName "a"))|]
  _ ->
    pure Nothing

msgpackEncodeConstraint :: Param -> Q (Maybe Type)
msgpackEncodeConstraint = \case
  Param _ _ (Just p) ->
    Just <$> [t|MsgpackEncode $(varT p)|]
  Param _ _ Nothing ->
    pure Nothing

effReturnType :: ApiType -> Q (Maybe Name, Type)
effReturnType = \case
  PolyType -> do
    let n = mkName "a"
    pure (Just n, VarT n)
  a -> do
    t <- reifyApiType a
    pure (Nothing, t)

analyzeReturnType :: ApiType -> Q (Maybe Name, Type, Maybe Type)
analyzeReturnType tpe = do
  (n, rt) <- effReturnType tpe
  constraint <- msgpackDecodeConstraint tpe
  pure (n, rt, constraint)

effectiveType :: ApiType -> Q Type
effectiveType = \case
  PolyType ->
    pure (VarT (mkName "a"))
  a ->
    reifyApiType a

dataSig :: [Param] -> Name -> ApiType -> DecQ
dataSig params name returnType = do
  (retTv, retType, decodeConstraint) <- analyzeReturnType returnType
  m <- newName "m"
  monadRpcConstraint <- [t|MonadRpc $(varT m)|]
  encodeConstraints <- traverse msgpackEncodeConstraint params
  let
    paramType = \case
      Param _ _ (Just n) ->
        VarT n
      Param _ t Nothing ->
        t
    paramsType =
      foldr (AppT . AppT ArrowT . paramType) (AppT (VarT m) retType) params
    constraints =
      monadRpcConstraint : maybeToList decodeConstraint <> catMaybes encodeConstraints
    paramTv = \case
      Param _ _ (Just n) ->
        Just n
      Param _ _ Nothing ->
        Nothing
    paramTvs =
      mapMaybe paramTv params
    tv n =
      KindedTV n SpecifiedSpec StarT
    mTv =
      KindedTV m SpecifiedSpec (AppT (AppT ArrowT StarT) StarT)
  sigD name (pure (ForallT ((tv <$> paramTvs) <> maybeToList (tv <$> retTv) <> [mTv]) constraints paramsType))

dataBody :: String -> Name -> [Param] -> DecQ
dataBody apiName name params =
  funD name [clause (varP <$> names) (normalB call) []]
  where
    call =
      [|rpcRequest (Request (RpcMethod apiName) $(listE (toObjVar <$> names)))|]
    toObjVar v =
      [|toMsgpack $(varE v)|]
    names =
      (.paramName) <$> params

genRequest :: MethodSpec -> DecsQ
genRequest (MethodSpec apiName name params returnType) = do
  sig <- dataSig params name returnType
  body <- dataBody apiName name params
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
        decodeIncompatible o
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
