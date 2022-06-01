module Ribosome.Host.IOStack where

import Conc (ConcStack)
import qualified Data.Text.IO as Text
import Polysemy.Chronos (ChronosTime, interpretTimeChronos)
import System.IO (stderr)

import Ribosome.Host.Data.BootError (BootError (BootError))

type IOStack =
  [
    ChronosTime,
    Error BootError
  ] ++ ConcStack

errorStderr :: IO (Either BootError ()) -> IO ()
errorStderr ma =
  ma >>= \case
    Left (BootError err) -> Text.hPutStrLn stderr err
    Right () -> unit

runIOStack ::
  Sem IOStack () ->
  IO ()
runIOStack =
  errorStderr .
  runConc .
  errorToIOFinal .
  interpretTimeChronos
