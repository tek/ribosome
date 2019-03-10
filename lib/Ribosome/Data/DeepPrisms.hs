{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Data.DeepPrisms where

import Control.Lens (Prism', makeClassyPrisms)
import qualified Control.Lens as Lens (preview, review)
import Control.Monad (join, (<=<))
import Language.Haskell.TH
import Language.Haskell.TH.Datatype (
  ConstructorInfo(constructorName, constructorFields),
  DatatypeInfo(datatypeCons, datatypeName),
  reifyDatatype,
  )
import Language.Haskell.TH.Syntax (ModName(..), Name(Name), NameFlavour(NameQ, NameS, NameG), OccName(..))

class DeepPrisms e e' where
  prism :: Prism' e e'

hoist :: DeepPrisms e e' => e' -> e
hoist =
  Lens.review prism

retrieve :: DeepPrisms e e' => e -> Maybe e'
retrieve =
  Lens.preview prism

data Ctor =
  Ctor {
    ctorName :: Name,
    ctorTypes :: [Type]
  }

data SubError =
  SubError {
    seCtor :: Name,
    seWrapped :: Name
  }

ctor :: ConstructorInfo -> Ctor
ctor info = Ctor (constructorName info) (constructorFields info)

data DT =
  DT {
    dtName :: Name,
    dtCons :: [Ctor]
  }

dataType :: Name -> Q DT
dataType name = do
  info <- reifyDatatype name
  return $ DT (datatypeName info) (ctor <$> datatypeCons info)

mkHoist :: TypeQ -> TypeQ -> BodyQ -> DecQ
mkHoist _ _ body = do
  (VarE name) <- [|prism|]
  funD name [clause [] body []]

deepPrismsInstance :: TypeQ -> TypeQ -> BodyQ -> DecQ
deepPrismsInstance top local body =
  instanceD (cxt []) (appT (appT [t|DeepPrisms|] top) local) [mkHoist top local body]

idInstance :: Name -> DecQ
idInstance name =
  deepPrismsInstance nt nt body
  where
    nt = conT name
    body = normalB [|id|]

eligibleForDeepError :: Name -> Q Bool
eligibleForDeepError tpe = do
  (ConT name) <- [t|DeepPrisms|]
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

prismName :: Name -> Name -> ExpQ
prismName (Name _ topFlavour) (Name (OccName n) prismFlavour) =
  varE (Name (OccName ('_' : n)) flavour)
  where
    flavour
      | sameModule topFlavour prismFlavour = NameS
      | otherwise = prismFlavour

deepInstances :: Name -> [Name] -> Name -> Name -> DecsQ
deepInstances top intermediate name tpe = do
  current <- deepPrismsInstance (conT top) (conT tpe) (normalB body)
  sub <- subInstances top (name : intermediate) tpe
  return (current : sub)
  where
    compose = appE . appE [|(.)|] . prismName top
    body = foldr compose (prismName top name) intermediate

deepInstancesIfEligible :: Name -> [Name] -> Ctor -> DecsQ
deepInstancesIfEligible top intermediate (Ctor name [ConT tpe]) = do
  eligible <- eligibleForDeepError tpe
  if eligible then deepInstances top intermediate name tpe else return []
deepInstancesIfEligible _ _ _ =
  return []

errorInstances :: DT -> DecsQ
errorInstances (DT name cons) = do
  idInst <- idInstance name
  deepInsts <- traverse (deepInstancesIfEligible name []) cons
  return (idInst : join deepInsts)

deepPrisms' :: Name -> DecsQ
deepPrisms' =
  errorInstances <=< dataType

deepPrisms :: Name -> DecsQ
deepPrisms name = do
  prisms <- makeClassyPrisms name
  err <- deepPrisms' name
  return $ prisms ++ err
