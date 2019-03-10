{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Data.Deep where

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

class Deep e e' where
  prism :: Prism' e e'

hoist :: Deep e e' => e' -> e
hoist =
  Lens.review prism

retrieve :: Deep e e' => e -> Maybe e'
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
  instanceD (cxt []) (appT (appT [t|Deep|] top) local) [mkHoist top local body]

idInstance :: Name -> DecQ
idInstance name =
  deepPrismsInstance nt nt body
  where
    nt = conT name
    body = normalB [|id|]

eligibleForDeepError :: Name -> Q Bool
eligibleForDeepError tpe = do
  (ConT name) <- [t|Deep|]
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

-- FIXME replace unsafe head/tail
deepInstances :: Name -> [Name] -> Name -> Name -> DecsQ
deepInstances top intermediate name tpe = do
  current <- deepPrismsInstance (conT top) (conT tpe) body
  sub <- subInstances top newIntermediate tpe
  return (current : sub)
  where
    newIntermediate = name : intermediate
    ri = prismName top <$> newIntermediate
    body = normalB $ foldr (\a z -> appE (appE [|(.)|] a) z) (head ri) (tail ri)

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

fixPrismNames :: Dec -> Q Dec
fixPrismNames = return

deepPrisms :: Name -> DecsQ
deepPrisms name = do
  prisms <- makeClassyPrisms name
  prisms' <- traverse fixPrismNames prisms
  err <- deepPrisms' name
  return $ prisms' ++ err
