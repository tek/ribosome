module Ribosome.App.Error where

import Rainbow (Chunk, chunk, faint, fore, hPutChunksLn, red)
import System.IO (Handle)


newtype RainbowError =
  RainbowError { unRainbowError :: NonEmpty (NonEmpty Chunk) }
  deriving newtype (Semigroup)

instance IsString RainbowError where
  fromString =
    RainbowError . pure . pure . fromString

appError :: [Chunk] -> RainbowError
appError msg =
  RainbowError ["⚠️ " :| (fore red "Error ")  : msg]

ioError :: [Chunk] -> Text -> RainbowError
ioError msg err =
  appError msg <> RainbowError [["🗨️ ", fore red (faint (chunk err))]]

outputError ::
  Members [Stop RainbowError, Embed IO] r =>
  IO a ->
  Sem r a
outputError =
  stopTryIOError err
  where
    err =
      ioError ["Printing message failed"]

runRainbowErrorAnd ::
  Members [Embed IO, Final IO] r =>
  Handle ->
  Sem r () ->
  Sem (Stop RainbowError : r) () ->
  Sem r ()
runRainbowErrorAnd handle after action = do
  either onError pure =<< stopToIOFinal action
  where
    onError (RainbowError cs) = do
      tryIOError_ (traverse_ (hPutChunksLn handle . toList) cs)
      after

runRainbowError ::
  Members [Embed IO, Final IO] r =>
  Handle ->
  Sem (Stop RainbowError : r) () ->
  Sem r ()
runRainbowError handle =
  runRainbowErrorAnd handle unit
