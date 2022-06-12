module Ribosome.Host.Interpreter.Process.Socket where

import Data.Serialize (Serialize)
import qualified Network.Socket as Socket
import Network.Socket (socketToHandle)
import Path (toFilePath)
import Polysemy.Process (Process, ProcessOptions, interpretProcessHandles)
import Polysemy.Process.Data.ProcessError (ProcessError)
import System.IO (Handle, IOMode (ReadWriteMode))

import Ribosome.Host.Data.BootError (BootError (BootError))
import Ribosome.Host.Data.NvimSocket (NvimSocket (NvimSocket))
import Ribosome.Host.Interpreter.Process.Cereal (interpretProcessInputCereal, interpretProcessOutputCereal)

withSocket ::
  Members [Reader NvimSocket, Resource, Error BootError, Embed IO] r =>
  (Handle -> Sem r a) ->
  Sem r a
withSocket use =
  bracket acquire release \ socket ->
    use =<< embed (socketToHandle socket ReadWriteMode)
  where
    acquire = do
      NvimSocket path <- ask
      fromEither . first BootError =<< tryAny do
        socket <- Socket.socket Socket.AF_UNIX Socket.Stream 0
        socket <$ Socket.connect socket (Socket.SockAddrUnix (toFilePath path))
    release =
      tryAny_ . Socket.close

interpretProcessCerealSocket ::
  ∀ a r .
  Serialize a =>
  Members [Reader NvimSocket, Error BootError, Log, Resource, Race, Async, Embed IO] r =>
  ProcessOptions ->
  InterpreterFor (Process a (Either Text a) !! ProcessError) r
interpretProcessCerealSocket options sem =
  withSocket \ handle ->
    interpretProcessOutputCereal $
    interpretProcessInputCereal $
    interpretProcessHandles options handle handle (raiseUnder2 sem)
