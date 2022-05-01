module Ribosome.Host.TH.Api.Generate where

import Data.Char (toUpper)
import qualified Data.Map.Strict as Map (fromList, toList)
import Data.MessagePack (Object)
import Exon (exon)
import Language.Haskell.TH (
  Dec,
  DecsQ,
  Name,
  Q,
  Type,
  TypeQ,
  appT,
  conT,
  listT,
  mkName,
  newName,
  runIO,
  tupleT,
  )
import Prelude hiding (Type)

import qualified Ribosome.Host.Data.ApiInfo as ApiInfo
import Ribosome.Host.Data.ApiInfo (ApiInfo (ApiInfo), ExtType, ExtTypeMeta, RpcDecl (RpcDecl), apiInfo, unExtType)
import Ribosome.Host.Data.ApiType (ApiPrim (..), ApiType (..))
import Ribosome.Host.Data.LuaRef (LuaRef)

camelcase :: String -> String
camelcase =
  snd . foldr folder (False, "")
  where
    folder '_' (_, z) = (True, z)
    folder a (True, h : t) = (False, a : toUpper h : t)
    folder a (True, []) = (False, [a])
    folder a (False, z) = (False, a : z)

haskellTypes :: Map String TypeQ
haskellTypes =
  Map.fromList [
    ("Boolean", [t|Bool|]),
    ("Integer", [t|Int|]),
    ("Float", [t|Double|]),
    ("String", [t|Text|]),
    ("Array", [t|[Object]|]),
    ("Dictionary", [t|Map Text Object|]),
    ("void", [t|()|])
    ]

reifyApiPrim :: ApiPrim -> Q Type
reifyApiPrim = \case
  Boolean -> [t|Bool|]
  Integer -> [t|Int|]
  Float -> [t|Double|]
  String -> [t|Text|]
  Dictionary -> [t|Map Text Object|]
  Object -> [t|Object|]
  Void -> [t|()|]
  LuaRef -> [t|LuaRef|]

reifyApiType :: ApiType -> Q Type
reifyApiType = \case
  Prim t ->
    reifyApiPrim t
  Array t (Just count) ->
    foldl appT (tupleT count) (replicate count (reifyApiType t))
  Array t Nothing ->
    appT listT (reifyApiType t)
  Ext t ->
    conT (mkName t)

data MethodSpec =
  MethodSpec {
    apiName :: String,
    camelcaseName :: Name,
    paramNames :: [Name],
    paramTypes :: [Type],
    returnType :: ApiType
  }
  deriving stock (Eq, Show)

functionData :: RpcDecl -> Q MethodSpec
functionData (RpcDecl name parameters _ _ _ returnType) = do
  names <- traverse newName prefixedNames
  types <- traverse reifyApiType (fst <$> parameters)
  pure (MethodSpec name (mkName (camelcase name)) names types returnType)
  where
    prefix i n =
      [exon|arg#{show i}_#{n}|]
    prefixedNames =
      zipWith prefix [0 :: Int ..] (snd <$> parameters)

genExtTypes :: Map ExtType ExtTypeMeta -> (Name -> ExtTypeMeta -> DecsQ) -> Q [[Dec]]
genExtTypes types gen =
  traverse (uncurry gen) (first (mkName . unExtType) <$> Map.toList types)

generateFromApi :: (MethodSpec -> Q [Dec]) -> Maybe (Name -> ExtTypeMeta -> DecsQ) -> Q [Dec]
generateFromApi handleFunction handleExtType = do
  ApiInfo {functions, types} <- either (fail . show) pure =<< runIO apiInfo
  funcs <- traverse functionData functions
  funcDecs <- traverse handleFunction funcs
  tpeDecs <- traverse (genExtTypes types) handleExtType
  pure (join (funcDecs <> fold tpeDecs))
