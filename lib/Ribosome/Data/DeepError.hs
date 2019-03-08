{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Data.DeepError where

import Control.Monad (join, (<=<))
import Control.Monad.Error.Class (MonadError(throwError))
import Data.Foldable (fold)
import Language.Haskell.TH
import Language.Haskell.TH.Datatype (
  ConstructorInfo(constructorName, constructorFields),
  DatatypeInfo(datatypeCons, datatypeName),
  reifyDatatype,
  )

class DeepError e e' where
  hoist :: e' -> e

class (MonadError e m) => MonadDeepError e e' m where
  throwHoist :: e' -> m ()

instance (MonadError e m, DeepError e e') => MonadDeepError e e' m where
  throwHoist = throwError . hoist

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
mkHoist _ _ body =
  funD (mkName "hoist") [clause [varP . mkName $ "e'"] body []]

deepErrorInstance :: TypeQ -> TypeQ -> BodyQ -> DecQ
deepErrorInstance top local body =
  instanceD (cxt []) (appT (appT [t|DeepError|] top) local) [mkHoist top local body]

idInstance :: Name -> DecQ
idInstance name =
  deepErrorInstance nt nt body
  where
    nt = conT name
    body = normalB [|e'|]

forDeepError :: Name -> Q (Maybe Name)
forDeepError tpe = do
  eligible <- isInstance (mkName "Ribosome.Data.DeepError.DeepError") [ConT tpe, ConT tpe]
  return $ if eligible then Just tpe else Nothing

subInstances :: Name -> [Name] -> Name -> DecsQ
subInstances top intermediate local = do
  (DT _ subCons) <- dataType local
  join <$> traverse (deepInstances top intermediate) subCons

deepInstances :: Name -> [Name] -> Ctor -> DecsQ
deepInstances top intermediate (Ctor name [ConT tpe]) = do
  current <- deepErrorInstance (conT top) (conT tpe) body
  subEligible <- forDeepError tpe
  sub <- fold <$> traverse (subInstances top newIntermediate) subEligible
  return (current : sub)
  where
    newIntermediate = name : intermediate
    body = normalB $ foldr appE [|e'|] (conE <$> reverse newIntermediate)
deepInstances _ _ _ =
  return []

errorInstances :: DT -> DecsQ
errorInstances (DT name cons) = do
  idInst <- idInstance name
  deepInsts <- traverse (deepInstances name []) cons
  return (idInst : join deepInsts)

deepError' :: Name -> DecsQ
deepError' =
  errorInstances <=< dataType

deepError :: Name -> DecsQ
deepError name = do
  let basic = []
  -- basic <- makeClassyPrisms name
  err <- deepError' name
  return $ basic ++ err
