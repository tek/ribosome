module Ribosome.Errors (
  module Ribosome.Errors,
  module Ribosome.Host.Effect.Errors,
) where

import Log (dataLog)

import Ribosome.Data.SettingError (SettingError)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Data.HandlerError (
  HandlerError,
  HandlerTag (GlobalTag),
  ToErrorMessage,
  resumeHandlerError,
  toHandlerError,
  )
import Ribosome.Host.Data.HostError (HostError (HostError))
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Errors
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.IOStack (TestEffects)

storeError ::
  ToErrorMessage e =>
  Member (DataLog HostError) r =>
  Bool ->
  Maybe HandlerTag ->
  e ->
  Sem r ()
storeError report htag (toHandlerError (fromMaybe GlobalTag htag) -> err) =
  withFrozenCallStack do
    dataLog (HostError report err)

reportError ::
  ToErrorMessage e =>
  Member (DataLog HostError) r =>
  Maybe HandlerTag ->
  e ->
  Sem r ()
reportError htag e =
  withFrozenCallStack do
    storeError True htag e

pluginHandlerErrors ::
  Members [Scratch !! RpcError, Settings !! SettingError, Rpc !! RpcError, Stop HandlerError] r =>
  InterpretersFor TestEffects r
pluginHandlerErrors =
  resumeHandlerError @Rpc .
  resumeHandlerError @Settings .
  resumeHandlerError @Scratch
