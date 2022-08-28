module Ribosome.App.UserInput where

import qualified Data.Text.IO as Text
import Path (Path)
import Rainbow (
  Chunk,
  Radiant,
  blue,
  bold,
  chunk,
  color256,
  faint,
  fore,
  green,
  hPutChunks,
  hPutChunksLn,
  magenta,
  only256,
  yellow,
  )
import System.IO (getLine, stderr)

import Ribosome.App.Error (RainbowError, appError, outputError)
import Ribosome.Host.Path (pathText)

color :: Radiant -> Word8 -> Chunk -> Chunk
color r c =
  fore (r <> only256 (color256 c))

fbColor :: Radiant -> Word8 -> Chunk -> Chunk
fbColor r c =
  color r c . faint . bold

pathColor :: Chunk -> Chunk
pathColor =
  fbColor yellow 172

cmdColor :: Chunk -> Chunk
cmdColor =
  fbColor blue 111

pathChunk :: Path b t -> Chunk
pathChunk path =
  pathColor (chunk (pathText path))

neovimChunk :: Chunk
neovimChunk =
  fbColor green 76 "Neovim"

linkChunk :: Text -> Chunk
linkChunk =
  fbColor blue 33 . chunk

putStderr ::
  Member (Embed IO) r =>
  Text ->
  Sem r ()
putStderr =
  embed . Text.hPutStrLn stderr

infoMessage ::
  Members [Stop RainbowError, Embed IO] r =>
  [Chunk] ->
  Sem r ()
infoMessage cs =
  outputError (hPutChunksLn stderr (color magenta 55 (bold ">>= ") : cs))

askUser ::
  Eq a =>
  IsString a =>
  Members [Stop RainbowError, Embed IO] r =>
  Text ->
  Sem r (Maybe a)
askUser msg = do
  infoMessage [fore blue (chunk msg)]
  outputError (hPutChunks stderr ["✍️", fore magenta (bold " > ")])
  check . fromString <$> stopTryIOError (const ("" <> appError ["Aborted."])) getLine
  where
    check = \case
      "" -> Nothing
      a -> Just a

askRequired ::
  Eq a =>
  IsString a =>
  Members [Stop RainbowError, Embed IO] r =>
  Text ->
  Sem r a
askRequired msg =
  askUser msg >>= \case
    Just a -> pure a
    Nothing -> do
      infoMessage [fore magenta (faint "This option is mandatory.")]
      askRequired msg
