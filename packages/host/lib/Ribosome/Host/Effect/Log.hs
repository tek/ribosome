module Ribosome.Host.Effect.Log where

type StderrLog =
  Tagged "stderr" Log

type FileLog =
  Tagged "file" Log

stderrLog ::
  Member StderrLog r =>
  InterpreterFor Log r
stderrLog =
  tag @"stderr"

fileLog ::
  Member FileLog r =>
  InterpreterFor Log r
fileLog =
  tag @"file"
