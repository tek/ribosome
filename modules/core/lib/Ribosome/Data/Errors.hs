{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Data.Errors(
  ComponentName(..),
  Errors(..),
  Error(..),
  componentErrors,
  timestamp,
  report,
) where

import qualified Data.Map.Strict as Map (toList)
import Data.Text.Prettyprint.Doc (Doc, Pretty(..), align, line, vsep, (<+>))
import Prelude hiding (error)

import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))

newtype ComponentName =
  ComponentName Text
  deriving (Eq, Ord, Show)

data Error =
  Error {
    _timestamp :: Int,
    _report :: ErrorReport
  }
  deriving (Eq, Show)

makeClassy ''Error

instance Pretty Error where
  pretty (Error stamp (ErrorReport _ lines' _)) =
    pretty stamp <+> align (vsep (pretty <$> lines'))

newtype Errors =
  Errors {
    _componentErrors :: Map ComponentName [Error]
    }
  deriving (Eq, Show, Default)

makeClassy ''Errors

prettyComponentErrors :: ComponentName -> [Error] -> Doc a
prettyComponentErrors (ComponentName name) errors' =
  line <> line <> pretty name <> ":" <> line <> vsep (pretty <$> errors')

instance Pretty Errors where
  pretty (Errors errors') =
    vsep (uncurry prettyComponentErrors <$> Map.toList errors')
