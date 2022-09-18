module Ribosome.Host.Interpreter.Process.Cereal where

import qualified Data.ByteString as ByteString
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
  interpretProcessOutputIncremental,
  interpretProcess_,
  interpretSystemProcessNative_, SystemProcessScopeError,
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

type Parser a =
  ByteString -> ProcessOutputParseResult a

interpretProcessOutputCereal ::
  ∀ a r .
  Serialize a =>
  InterpreterFor (ProcessOutput 'Stdout (Either Text a)) r
interpretProcessOutputCereal =
  interpretProcessOutputIncremental (convertResult . runGetPartial Serialize.get)

interpretProcessOutputLog ::
  ∀ p a r .
  Member Log r =>
  InterpreterFor (ProcessOutput p a) r
interpretProcessOutputLog =
  interpret \case
    Chunk _ msg ->
      ([], "") <$ unless (ByteString.null msg) (Log.debug [exon|Nvim stderr: #{decodeUtf8 msg}|])

interpretProcessInputCereal ::
  Serialize a =>
  InterpreterFor (ProcessInput a) r
interpretProcessInputCereal =
  interpret \case
    ProcessInput.Encode msg ->
      pure (Serialize.encode msg)

interpretProcessCereal ::
  ∀ resource a r .
  Serialize a =>
  Member (Scoped_ resource (SystemProcess !! SystemProcessError) !! SystemProcessScopeError) r =>
  Members [Log, Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  InterpreterFor (Scoped_ () (Process a (Either Text a)) !! ProcessError) r
interpretProcessCereal options =
  interpretProcessOutputLog @'Stderr .
  interpretProcessOutputCereal .
  interpretProcessInputCereal .
  interpretProcess_ @resource options .
  raiseUnder3

interpretProcessCerealNative ::
  ∀ a r .
  Serialize a =>
  Members [Log, Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  ProcessConfig () () () ->
  InterpreterFor (Scoped_ () (Process a (Either Text a)) !! ProcessError) r
interpretProcessCerealNative options conf =
  interpretSystemProcessNative_ conf .
  interpretProcessCereal @PipesProcess @a options .
  raiseUnder
