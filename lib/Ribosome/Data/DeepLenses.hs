{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Data.DeepLenses where

import Control.Applicative (liftA2)
import Control.Lens (Lens', makeClassy)
import Control.Monad (join, (<=<))
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

data SubError =
  SubError {
    seCtor :: Name,
    seWrapped :: Name
  }

data DT =
  DT {
    dtName :: Name,
    dtFields :: [Field]
  }

dataType :: Name -> Q DT
dataType name = do
  info <- reifyDatatype name
  return $ DT (datatypeName info) (fields $ datatypeCons info)
  where
    fields [ConstructorInfo _ _ _ types _ (RecordConstructor names)] =
      liftA2 Field names types
    fields _ =
      []

mkHoist :: TypeQ -> TypeQ -> BodyQ -> DecQ
mkHoist _ _ body = do
  (VarE name) <- [|deepLens|]
  funD name [clause [] body []]

deepLensesInstance :: TypeQ -> TypeQ -> BodyQ -> DecQ
deepLensesInstance top local body =
  instanceD (cxt []) (appT (appT [t|DeepLenses|] top) local) [mkHoist top local body]

idInstance :: Name -> DecQ
idInstance name =
  deepLensesInstance nt nt body
  where
    nt = conT name
    body = normalB [|id|]

eligibleForDeepError :: Name -> Q Bool
eligibleForDeepError tpe = do
  (ConT name) <- [t|DeepLenses|]
  isInstance name [ConT tpe, ConT tpe]

subInstances :: Name -> [Name] -> Name -> DecsQ
subInstances top intermediate local = do
  (DT _ subCons) <- dataType local
  join <$> traverse (deepInstancesIfEligible top intermediate) subCons

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

deepInstances :: Name -> [Name] -> Name -> Name -> DecsQ
deepInstances top intermediate name tpe = do
  current <- deepLensesInstance (conT top) (conT tpe) (normalB body)
  sub <- subInstances top (name : intermediate) tpe
  return (current : sub)
  where
    compose = appE . appE [|(.)|] . lensName top
    body = foldr compose (lensName top name) intermediate

deepInstancesIfEligible :: Name -> [Name] -> Field -> DecsQ
deepInstancesIfEligible top intermediate (Field name (ConT tpe)) = do
  eligible <- eligibleForDeepError tpe
  if eligible then deepInstances top intermediate name tpe else return []
deepInstancesIfEligible _ _ _ =
  return []

errorInstances :: DT -> DecsQ
errorInstances (DT name fields) = do
  idInst <- idInstance name
  deepInsts <- traverse (deepInstancesIfEligible name []) fields
  return (idInst : join deepInsts)

deepLenses' :: Name -> DecsQ
deepLenses' =
  errorInstances <=< dataType

deepLenses :: Name -> DecsQ
deepLenses name = do
  lenses <- makeClassy name
  err <- deepLenses' name
  return $ lenses ++ err
