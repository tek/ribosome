module Ribosome.Host.TH.Api.Param where

import Language.Haskell.TH (Name, Type)
import Prelude hiding (Type)

data Param =
  Param { paramName :: Name, monoType :: Type, polyName :: Maybe Name }
  deriving stock (Eq, Show)
