module Ribosome.Host.TH.Api.GenerateEffect where

import Language.Haskell.TH (
  Dec,
  DecQ,
  Name,
  Q,
  Quote (newName),
  Type (AppT, ArrowT, ForallT, VarT),
  TypeQ,
  appE,
  clause,
  funD,
  mkName,
  normalB,
  sigD,
  varE,
  varP,
  varT,
  )
import Prelude hiding (Type)

import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode)
import Ribosome.Host.Data.ApiType (ApiPrim (Object), ApiType (Prim))
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.TH.Api.Generate (MethodSpec (MethodSpec), generateFromApi, reifyApiType)

msgpackDecodeConstraint :: ApiType -> Q (Maybe Type)
msgpackDecodeConstraint = \case
  Prim Object ->
    Just <$> [t|MsgpackDecode $(varT (mkName "a"))|]
  _ ->
    pure Nothing

newT :: String -> TypeQ
newT =
  varT <=< newName

ioReturnType :: ApiType -> Q Type
ioReturnType = \case
  Prim Object ->
    pure (VarT (mkName "a"))
  a ->
    reifyApiType a

analyzeReturnType :: ApiType -> Q (Type, Maybe Type)
analyzeReturnType tpe = do
  rt <- ioReturnType tpe
  constraint <- msgpackDecodeConstraint tpe
  pure (rt, constraint)

ioSig :: Name -> [Type] -> ApiType -> DecQ
ioSig name types returnType = do
  stack <- newT "r"
  rpcConstraint <- [t|Member Rpc $(pure stack)|]
  (retType, decodeConstraint) <- analyzeReturnType returnType
  semT <- [t|Sem|]
  let
    params = foldr (AppT . AppT ArrowT) (AppT (AppT semT stack) retType) types
    constraints = rpcConstraint : maybeToList decodeConstraint
  sigD name $ pure (ForallT [] constraints params)

ioBody :: Name -> [Name] -> DecQ
ioBody name names =
  funD name [clause (varP <$> names) (normalB effectCons) []]
  where
    effectCons = appE [|Rpc.sync|] args
    args = foldl appE (varE $ mkName $ "RpcData." <> show name) (varE <$> names)

genMethod :: MethodSpec -> Q [Dec]
genMethod (MethodSpec _ name names types returnType) = do
  sig <- ioSig name types returnType
  body <- ioBody name names
  pure [sig, body]

generateEffect :: Q [Dec]
generateEffect =
  generateFromApi genMethod Nothing
