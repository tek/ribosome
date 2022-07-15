module Ribosome.App.Templates.PluginHs where

import Exon (exon)

import Ribosome.App.Data (ModuleName (ModuleName), ProjectName (ProjectName))

pluginHs :: ProjectName -> ModuleName -> Text
pluginHs (ProjectName name) (ModuleName modName) =
  [exon|module #{modName}.Plugin where

import Conc (interpretAtomic, withAsync_)
import Ribosome (
  Execution (Sync),
  Handler,
  RpcHandler,
  rpcFunction,
  runNvimHandlersIO,
  )

type #{modName}Stack =
  '[
    AtomicState Int
  ]

ping ::
  Member (AtomicState Int) r =>
  Handler r Int
ping =
  atomicState' \ s -> (s + 1, s + 1)

handlers ::
  Member (AtomicState Int) r =>
  [RpcHandler r]
handlers =
  [
    rpcFunction "#{modName}Ping" Sync ping
  ]

prepare ::
  Sem r ()
prepare =
  unit

interpret#{modName}Stack ::
  Members [Resource, Race, Async, Embed IO] r =>
  InterpretersFor #{modName}Stack r
interpret#{modName}Stack sem =
  interpretAtomic 0 do
    withAsync_ prepare sem

main :: IO ()
main =
  runNvimHandlersIO @#{modName}Stack "#{name}" interpret#{modName}Stack handlers
|]
