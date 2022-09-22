{-# options_haddock prune #-}

-- |The syntax DSL algebra.
module Ribosome.Data.Syntax.Dsl where

import Ribosome.Data.SyntaxItem (SyntaxGroup, SyntaxItem)

-- |The syntax DSL algebra.
data Alg :: Type where
  Item :: SyntaxItem -> Alg
  Choice :: Alg -> Alg -> Alg
  Chain :: Alg -> Alg -> Alg
  Contain :: Alg -> Alg -> Alg
  Prefix :: SyntaxGroup -> Alg -> Alg
  Hi :: Map Text Text -> Alg -> Alg
  Link :: SyntaxGroup -> Alg -> Alg
  Mod :: (SyntaxItem -> SyntaxItem) -> Alg -> Alg

data Building =
  Building {
    prefix :: SyntaxGroup,
    contained :: Bool,
    next :: [SyntaxGroup],
    contains :: [SyntaxGroup],
    modItem :: SyntaxItem -> SyntaxItem
  }
  deriving stock (Generic)

instance Default Building where
  def =
    Building "" False [] [] id
