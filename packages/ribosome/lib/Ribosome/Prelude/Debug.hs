{-# language NoImplicitPrelude #-}

module Ribosome.Prelude.Debug where

import qualified Data.Text as Text
import Exon (exon)
import GHC.Stack (SrcLoc (..))
import Relude
import System.IO.Unsafe (unsafePerformIO)

srcLoc :: CallStack -> SrcLoc
srcLoc = \case
  (getCallStack -> (_, loc) : _) -> loc
  _ -> error "Debug.srcLoc: empty CallStack"

debugPrint ::
  SrcLoc ->
  Text ->
  IO ()
debugPrint SrcLoc{ srcLocModule = (toText -> slm), ..} msg =
  putStrLn [exon|#{toString moduleName}:#{show srcLocStartLine} #{toString msg}|]
  where
    moduleName =
      fromMaybe slm $ listToMaybe $ reverse $ Text.splitOn "." slm

debugPrintWithLoc ::
  Monad m =>
  SrcLoc ->
  Text ->
  m ()
debugPrintWithLoc loc msg = do
  () <- return (unsafePerformIO (debugPrint loc msg))
  pure ()

dbg ::
  HasCallStack =>
  Monad m =>
  Text ->
  m ()
dbg =
  debugPrintWithLoc (srcLoc callStack)
{-# inline dbg #-}

dbgsWith ::
  HasCallStack =>
  Monad m =>
  Show a =>
  Text ->
  a ->
  m ()
dbgsWith prefix a =
  debugPrintWithLoc (srcLoc callStack) [exon|#{prefix}: #{show @Text a}|]
{-# inline dbgsWith #-}

dbgs ::
  HasCallStack =>
  Monad m =>
  Show a =>
  a ->
  m ()
dbgs a =
  debugPrintWithLoc (srcLoc callStack) (show a)
{-# inline dbgs_ #-}

dbgs_ ::
  HasCallStack =>
  Monad m =>
  Show a =>
  a ->
  m a
dbgs_ a =
  a <$ debugPrintWithLoc (srcLoc callStack) (show a)
{-# inline dbgs #-}

tr ::
  HasCallStack =>
  Text ->
  a ->
  a
tr msg a =
  unsafePerformIO (a <$ debugPrint (srcLoc callStack) msg)
{-# inline tr #-}

trs ::
  Show a =>
  HasCallStack =>
  a ->
  a
trs a =
  unsafePerformIO (a <$ debugPrint (srcLoc callStack) (show a))
{-# inline trs #-}

trs' ::
  Show b =>
  HasCallStack =>
  b ->
  a ->
  a
trs' b a =
  unsafePerformIO (a <$ debugPrint (srcLoc callStack) (show b))
{-# inline trs' #-}
