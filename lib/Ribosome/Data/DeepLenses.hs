{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Data.DeepLenses where

import Control.Lens (Lens', makeClassy)
import Control.Monad (join)
import Data.List (zipWith)
import Language.Haskell.TH
import Language.Haskell.TH.Datatype (
  ConstructorInfo(ConstructorInfo),
  ConstructorVariant(RecordConstructor),
  DatatypeInfo(datatypeCons, datatypeName),
  reifyDatatype,
  )
import Language.Haskell.TH.Syntax (ModName(..), Name(Name), NameFlavour(NameQ, NameS, NameG), OccName(..))

class DeepLenses e e' where
  deepLens :: Lens' e e'

data Field =
  Field {
    fieldName :: Name,
    fieldType :: Type
  }
  deriving Show

data DT =
  DT {
    dtName :: Name,
    dtFields :: [Field]
  }
  deriving Show

dataType :: Name -> Q DT
dataType name = do
  info <- reifyDatatype name
  return $ DT (datatypeName info) (fields $ datatypeCons info)
  where
    fields [ConstructorInfo _ _ _ types _ (RecordConstructor names)] =
      zipWith Field names types
    fields _ =
      []

mkHoist :: TypeQ -> TypeQ -> BodyQ -> DecQ
mkHoist _ _ body = do
  (VarE name) <- [|deepLens|]
  funD name [clause [] body []]

deepLensesInstance :: TypeQ -> TypeQ -> BodyQ -> DecQ
deepLensesInstance top local body =
  instanceD (cxt []) (appT (appT [t|DeepLenses|] top) local) [mkHoist top local body]

idLenses :: Name -> DecQ
idLenses name =
  deepLensesInstance nt nt body
  where
    nt = conT name
    body = normalB [|id|]

eligibleForDeepError :: Name -> Q Bool
eligibleForDeepError tpe = do
  (ConT name) <- [t|DeepLenses|]
  isInstance name [ConT tpe, ConT tpe]

modName :: NameFlavour -> Maybe ModName
modName (NameQ mod') =
  Just mod'
modName (NameG _ _ mod') =
  Just mod'
modName _ =
  Nothing

sameModule :: NameFlavour -> NameFlavour -> Bool
sameModule f1 f2 =
  case (modName f1, modName f2) of
    (Just a, Just b) | a == b -> True
    _ -> False

lensName :: Name -> Name -> ExpQ
lensName (Name _ topFlavour) (Name (OccName n) lensFlavour) =
  varE (Name (OccName (lensName' n)) flavour)
  where
    lensName' ('_' : t) = t
    lensName' [] = []
    lensName' a = a
    flavour
      | sameModule topFlavour lensFlavour = NameS
      | otherwise = lensFlavour

fieldLenses :: Name -> [Name] -> Field -> DecsQ
fieldLenses top intermediate (Field name (ConT tpe)) = do
  current <- deepLensesInstance (conT top) (conT tpe) (normalB body)
  sub <- dataLensesIfEligible top (name : intermediate) tpe
  return (current : sub)
  where
    compose = appE . appE [|(.)|] . lensName top
    body = foldr compose (lensName top name) (reverse intermediate)
fieldLenses _ _ _ =
  return []

dataLenses :: Name -> [Name] -> Name -> DecsQ
dataLenses top intermediate local = do
  (DT _ fields) <- dataType local
  join <$> traverse (fieldLenses top intermediate) fields

dataLensesIfEligible :: Name -> [Name] -> Name -> DecsQ
dataLensesIfEligible top intermediate local = do
  eligible <- eligibleForDeepError local
  if eligible then dataLenses top intermediate local else return []

lensesForMainData :: Name -> DecsQ
lensesForMainData name = do
  idL <- idLenses name
  fields <- dataLenses name [] name
  return (idL : fields)

deepLenses :: Name -> DecsQ
deepLenses name = do
  lenses <- makeClassy name
  err <- lensesForMainData name
  return $ lenses ++ err
