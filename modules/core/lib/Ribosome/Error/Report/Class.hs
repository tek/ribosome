module Ribosome.Error.Report.Class where

import qualified Data.Text as Text (pack)
import GHC.Generics (
  C1,
  Constructor,
  D1,
  Generic,
  K1(..),
  M1(..),
  Rep,
  S1,
  Selector,
  conIsRecord,
  from,
  selName,
  (:*:)(..),
  (:+:)(..),
  )
import System.Log.Logger (Priority(NOTICE, DEBUG))

import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))

class ReportError a where
  errorReport :: a -> ErrorReport
  default errorReport ::
    Generic a =>
    GenReportError (Rep a) =>
    a ->
    ErrorReport
  errorReport =
    genErrorReport . from

class GenReportError f where
  genErrorReport :: f a -> ErrorReport

instance GenReportError f => GenReportError (M1 i c f) where
  genErrorReport =
    genErrorReport . unM1

instance (GenReportError f, GenReportError g) => GenReportError (f :+: g) where
  genErrorReport (L1 a) =
    genErrorReport a
  genErrorReport (R1 a) =
    genErrorReport a

instance ReportError a => GenReportError (K1 i a) where
  genErrorReport =
    errorReport . unK1

instance ReportError [Char] where
  errorReport msg = ErrorReport (Text.pack msg) (Text.pack <$> [msg]) NOTICE

instance ReportError [[Char]] where
  errorReport (msg : extra) = ErrorReport (Text.pack msg) (Text.pack <$> (msg : extra)) NOTICE
  errorReport [] = ErrorReport "empty error" ["empty error"] DEBUG

instance ReportError Text where
  errorReport msg = ErrorReport msg [msg] NOTICE

instance ReportError [Text] where
  errorReport (msg : extra) =
    ErrorReport msg (msg : extra) NOTICE
  errorReport [] = ErrorReport "empty error" ["empty error"] DEBUG

instance ReportError () where
  errorReport _ = ErrorReport "" [] DEBUG
