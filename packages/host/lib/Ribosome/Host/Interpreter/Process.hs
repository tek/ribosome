module Ribosome.Host.Interpreter.Process where

import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize, runGetPartial)
import Exon (exon)
import qualified Polysemy.Log as Log
import Polysemy.Process (
  OutputPipe (Stderr, Stdout),
  Process,
  ProcessInput,
  ProcessOptions,
  ProcessOutputParseResult (Done, Fail, Partial),
  SystemProcess,
  interpretProcess,
  interpretProcessOutputIncremental,
  interpretSystemProcessNative,
  )
import Polysemy.Process.Data.ProcessError (ProcessError)
import Polysemy.Process.Data.SystemProcessError (SystemProcessError)
import qualified Polysemy.Process.Effect.ProcessInput as ProcessInput
import Polysemy.Process.Effect.ProcessOutput (ProcessOutput (Chunk))
import Polysemy.Process.Interpreter.SystemProcess (PipesProcess)
import System.Process.Typed (ProcessConfig)

convertResult :: Serialize.Result a -> ProcessOutputParseResult a
convertResult = \case
  Serialize.Fail err _ ->
    Fail (toText err)
  Serialize.Done a leftover ->
    Done a leftover
  Serialize.Partial cont ->
    Partial (convertResult . cont)

interpretProcessOutputCereal ::
  ∀ a r .
  Serialize a =>
  InterpreterFor (ProcessOutput 'Stdout (Either Text a)) r
interpretProcessOutputCereal =
  interpretProcessOutputIncremental (convertResult <$> runGetPartial Serialize.get)

interpretProcessOutputLog ::
  ∀ p a r .
  Member Log r =>
  InterpreterFor (ProcessOutput p a) r
interpretProcessOutputLog =
  interpret \case
    Chunk _ msg ->
      ([], "") <$ Log.error [exon|Nvim stderr: #{decodeUtf8 msg}|]

interpretProcessInputCereal ::
  Serialize a =>
  InterpreterFor (ProcessInput a) r
interpretProcessInputCereal =
  interpret \case
    ProcessInput.Encode msg ->
      pure (Serialize.encode msg)

interpretProcessCereal ::
  ∀ resource a err r .
  Serialize a =>
  Members [Scoped resource (SystemProcess !! err), Log, Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  InterpreterFor (Scoped () (Process a (Either Text a)) !! ProcessError) r
interpretProcessCereal options =
  interpretProcessOutputLog @'Stderr .
  interpretProcessOutputCereal .
  interpretProcessInputCereal .
  interpretProcess @resource @err options .
  raiseUnder3

interpretProcessCerealNative ::
  ∀ a r .
  Serialize a =>
  Members [Log, Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  ProcessConfig () () () ->
  InterpreterFor (Scoped () (Process a (Either Text a)) !! ProcessError) r
interpretProcessCerealNative options conf =
  interpretSystemProcessNative conf .
  interpretProcessCereal @PipesProcess @a @SystemProcessError options .
  raiseUnder
